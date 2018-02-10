////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Preprocessor implementation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import java.io.File

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.Map
import scala.collection.immutable
import scala.collection.mutable
import scala.util.Try

import com.argondesign.alogic.FindFile
import com.argondesign.alogic.antlr.AntlrConverters.RichParserRuleContext
import com.argondesign.alogic.antlr.AntlrConverters.RichToken
import com.argondesign.alogic.antlr.AntlrConverters.terminalNodeToRichToken
import com.argondesign.alogic.antlr.ParserErrorListener
import com.argondesign.alogic.antlr.VPreprocLexer
import com.argondesign.alogic.antlr.VPreprocParser
import com.argondesign.alogic.antlr.VPreprocParser.AnythingContext
import com.argondesign.alogic.antlr.VPreprocParser.BlockCommentContext
import com.argondesign.alogic.antlr.VPreprocParser.EntitiesContext
import com.argondesign.alogic.antlr.VPreprocParser.HashDefineContext
import com.argondesign.alogic.antlr.VPreprocParser.HashIfContext
import com.argondesign.alogic.antlr.VPreprocParser.HashIncludeContext
import com.argondesign.alogic.antlr.VPreprocParser.IdentifierContext
import com.argondesign.alogic.antlr.VPreprocParser.LineCommentContext
import com.argondesign.alogic.antlr.VPreprocParser.LiteralContext
import com.argondesign.alogic.antlr.VPreprocParser.StartContext
import com.argondesign.alogic.antlr.VPreprocParserBaseVisitor
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream

class Preprocessor(implicit cc: CompilerContext) {

  // Private worker to use by recursive includes, returns defines as well
  private[this] def process(
    source:          Source,
    includeResovler: (Source, String) => Either[List[String], Source],
    initialDefines:  immutable.Map[String, String]
  ): (String, Map[String, String]) = {
    // text, final defines and the remappings
    val (text, defines, remaps) = {
      // Map of #define to substitution
      val defines = mutable.Map[String, String]() ++ initialDefines

      // Deferred line number remappings
      val remaps = mutable.Stack[(Range, File)]()

      object PreprocVisitor extends VPreprocParserBaseVisitor[String] {
        override def visitStart(ctx: StartContext) = visit(ctx.entities())

        override def visitEntities(ctx: EntitiesContext) = ctx.entity.asScala.map(visit) mkString ""

        override def visitHashDefine(ctx: HashDefineContext): String = {
          val s = ctx.VIDENTIFIER.text
          if (defines contains s) {
            cc.warning(ctx.loc, s"Redefined preprocessor identifier '$s'")
          }
          defines(s) = ctx.VREST.text.trim
          ""
        }

        override def visitIdentifier(ctx: IdentifierContext): String = {
          val ident = ctx.text
          defines.getOrElse(ident, ident)
        }

        override def visitLiteral(ctx: LiteralContext): String = ctx.text

        override def visitLineComment(ctx: LineCommentContext): String = ctx.text

        override def visitBlockComment(ctx: BlockCommentContext): String = ctx.text

        override def visitAnything(ctx: AnythingContext): String = ctx.text

        override def visitHashIf(ctx: HashIfContext): String = {
          val valueCond = ctx.ifcond.text.filterNot(_.isWhitespace) == "#if"
          val ident = ctx.IDENTIFIER.text

          val useElseOpt = if (valueCond) {
            if (defines contains ident) {
              val defineValue = defines(ident)
              Try(defineValue.toInt).toOption match {
                case Some(0) => Some(true)
                case Some(_) => Some(false)
                case None => {
                  cc.error(
                    ctx,
                    s"#if condition macro '$ident' must be defined as a single integer,",
                    s"not '$defineValue'"
                  )
                  None
                }
              }
            } else {
              cc.error(ctx, s"#if condition macro '$ident' is not defined")
              None
            }
          } else {
            Some(!(defines contains ident))
          }

          useElseOpt match {
            case Some(useElse) => {
              val hasElse = ctx.entities.asScala.length > 1

              val first = if (!useElse) {
                visit(ctx.entities(0))
              } else {
                "\n" * ctx.entities(0).text.count(_ == '\n')
              }

              val second = if (useElse && hasElse) {
                visit(ctx.entities(1))
              } else if (hasElse) {
                "\n" * ctx.entities(1).text.count(_ == '\n')
              } else {
                ""
              }

              first + second
            }
            case None => "/* Omitted due to error while preprocessing */"
          }
        }

        override def visitHashInclude(ctx: HashIncludeContext): String = {
          // Get the include path specifier
          val includeSpec = ctx.LITERAL.text.drop(1).dropRight(1)

          // Find the include file
          val includeSource = includeResovler(source, includeSpec) match {
            case Left(msgs)          => cc.fatal(ctx, msgs: _*)
            case Right(resultSource) => resultSource
          }

          // Process the include file in the current context
          val (text, newDefines) = process(includeSource, includeResovler, immutable.Map() ++ defines)

          // Add the new #defines from the included file, there is no need
          // to warn for redefinitions here, as we have passed in 'defines'
          // to the nested preprocessor, so we have already yielded all warnings
          defines ++= newDefines

          // Record that we need to remap these locations for error messages,
          // but defer doing so until the whole file is processed. This ensures
          // errors printed during preprocessing are still printed correctly
          val start = ctx.loc.line
          val end = start + text.count(_ == '\n')
          remaps.push((start until end, includeSource.file))

          // Yield the preprocessed text
          text
        }
      }

      // Create ANTLR input stream
      val inputStream = CharStreams.fromString(source.text, source.name)

      // Create the lexer
      val lexer = new VPreprocLexer(inputStream)
      val tokenStream = new CommonTokenStream(lexer)
      tokenStream.fill()

      // Create the parser
      val parser = new VPreprocParser(tokenStream)
      parser.removeErrorListeners()
      parser.addErrorListener(new ParserErrorListener)

      // Parse the the file
      val parseTree = parser.start()

      // Walk parse tree to do the work
      val text = PreprocVisitor.visit(parseTree)

      // Return processed contents as a string
      (text, defines.toMap, remaps.toList.reverse)
    }

    // Apply the deferred remappings to the source location map for this processed file
    val remapCumSum = remaps.scanLeft(0)(_ + _._1.size)
    for (((range, src), offset) <- remaps zip remapCumSum) {
      cc.remap(source.file, range.start + offset until range.end + offset, src)
    }

    (text, defines)
  }

  // Preprocess a source, given include resolver and initial defines
  def apply(
    src:             Source,
    initialDefines:  Map[String, String],
    includeResolver: (Source, String) => Either[List[String], Source]
  ): String = process(src, includeResolver, initialDefines.toMap)._1

  // Preprocess a source, given search paths and initial defines
  def apply(
    src:                Source,
    initialDefines:     Map[String, String],
    includeSearchPaths: List[File]
  ): String = {
    // Include file resolver that looks up include files in the search paths, or the
    // directory of the including file
    def includeResolver(source: Source, includeSpec: String): Either[List[String], Source] = {
      val includePath = new File(includeSpec)
      // Find the include file
      if (includePath.isAbsolute) {
        Left(List(s"""No absolute include paths allowed: "$includeSpec""""))
      } else {
        // Prepend the directory of the including file to the search path
        val searchPaths = Option(source.file.getParentFile) match {
          case Some(parent) => parent.getCanonicalFile() :: includeSearchPaths
          case None         => includeSearchPaths
        }

        // Look for the file
        FindFile(includeSpec, searchPaths) match {
          case Some(file) => Right(Source(file))
          case None => {
            Left(s"""Cannot find include file "$includeSpec". Looked in:""" :: (searchPaths map { _.toString }))
          }
        }
      }
    }

    process(src, includeResolver, initialDefines.toMap)._1
  }
}
