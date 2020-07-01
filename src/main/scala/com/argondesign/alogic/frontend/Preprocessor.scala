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

import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.antlr.PreprocParser._
import com.argondesign.alogic.antlr._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Source
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream

import scala.collection.Map
import scala.collection.immutable
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

class Preprocessor(implicit cc: CompilerContext) {

  // Private worker to use by recursive includes, returns defines as well
  private[this] def process(
      source: Source,
      includeResolver: (Source, String) => Either[List[String], Source],
      initialDefines: immutable.Map[String, String]
    ): (Source, Map[String, String]) = {
    // Map of #define to substitution
    val defines = mutable.Map[String, String]() ++ initialDefines

    object PreprocVisitor extends PreprocParserBaseVisitor[String] {
      override def visitStart(ctx: StartContext): String =
        visit(ctx.entities)

      override def visitEntities(ctx: EntitiesContext): String =
        ctx.entity.asScala.map(visit) mkString ""

      override def visitHashDefine(ctx: HashDefineContext): String = {
        val s = ctx.IDENTIFIER.txt
        if (defines contains s) {
          cc.warning(ctx, s"Redefined preprocessor identifier '$s'")
        }
        defines(s) = ctx.REST.txt.trim
        "" // Replace with empty string
      }

      override def visitIdentifier(ctx: IdentifierContext): String = {
        val ident = ctx.txt
        defines.getOrElse(ident, ident)
      }

      override def visitLiteral(ctx: LiteralContext): String = ctx.txt

      override def visitLineComment(ctx: LineCommentContext): String = ctx.txt

      override def visitBlockComment(ctx: BlockCommentContext): String = ctx.txt

      override def visitAnything(ctx: AnythingContext): String = ctx.txt

      override def visitHashIf(ctx: HashIfContext): String = {
        val valueCond = ctx.ifcond.txt.filterNot(_.isWhitespace) == "#if"
        val ident = ctx.IDENTIFIER.txt

        val useElseOpt = if (valueCond) {
          if (defines contains ident) {
            val defineValue = defines(ident)
            Try(defineValue.toInt).toOption match {
              case Some(0) => Some(true)
              case Some(_) => Some(false)
              case None =>
                cc.error(
                  ctx,
                  s"#if condition macro '$ident' must be defined as a single integer,",
                  s"not '$defineValue'"
                )
                None
            }
          } else {
            cc.error(ctx, s"#if condition macro '$ident' is not defined")
            None
          }
        } else {
          Some(!(defines contains ident))
        }

        useElseOpt match {
          case Some(useElse) =>
            val hasElse = ctx.entities.asScala.length > 1

            val first = if (!useElse) {
              visit(ctx.entities(0))
            } else {
              "\n" * ctx.entities(0).txt.count(_ == '\n')
            }

            val second = if (useElse && hasElse) {
              visit(ctx.entities(1))
            } else if (hasElse) {
              "\n" * ctx.entities(1).txt.count(_ == '\n')
            } else {
              ""
            }

            first + second
          case None => "/* Omitted due to error while preprocessing */"
        }
      }

      override def visitHashInclude(ctx: HashIncludeContext): String = {
        // Get the include path specifier
        val includeSpec = ctx.LITERAL.txt.drop(1).dropRight(1)

        // Find the include file
        val includeSource = includeResolver(source, includeSpec) match {
          case Right(resultSource) => resultSource
          case Left(msgs)          => cc.fatal(ctx, msgs: _*)
        }

        // Process the include file in the current context
        val (result, newDefines) =
          process(includeSource, includeResolver, immutable.Map() ++ defines)

        val text = result.text

        // Add the new #defines from the included file, there is no need
        // to warn for redefinitions here, as we have passed in 'defines'
        // to the nested preprocessor, so we have already yielded all warnings
        defines ++= newDefines

        // Yield the preprocessed text, wrapped in #line directives
        s"""#line 1 "${includeSource.name}"\n""" +
          text +
          (if (text.last != '\n') "\n" else "") + // Ensure text ends in \n
          s"""#line ${ctx.loc.line + 1} "${ctx.loc.file}"""" // No \n here, source has it
      }
    }

    // Create the lexer
    val lexer = new PreprocLexer(CharStreams.fromString(source.text, source.name))
    lexer.setTokenFactory(new AlogicTokenFactory(source))
    val tokenStream = new CommonTokenStream(lexer)

    // Create the parser
    val parser = new PreprocParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(new AlogicParseErrorListener)

    // Parse the the file
    val parseTree = parser.start()

    // Walk parse tree to do the work
    val text = PreprocVisitor.visit(parseTree)

    (Source(source.file, text), defines.toMap)
  }

  // Preprocess a source, given include resolver and initial defines
  def apply(
      src: Source,
      initialDefines: Map[String, String],
      includeResolver: (Source, String) => Either[List[String], Source]
    ): Source = {
    process(src, includeResolver, initialDefines.toMap)._1
  }

  // Preprocess a source, given search paths and initial defines
  def apply(
      src: Source,
      initialDefines: Map[String, String],
      includeSearchPaths: List[File]
    ): Source = {
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
          case Some(parent) => parent.getCanonicalFile :: includeSearchPaths
          case None         => includeSearchPaths
        }

        // Look for the file
        FindFile(includeSpec, searchPaths, maxDepth = 1) match {
          case Some(file) => Right(Source(file))
          case None =>
            Left(s"""Cannot find include file "$includeSpec". Looked in:""" :: (searchPaths map {
              _.toString
            }))
        }
      }
    }

    process(src, includeResolver, initialDefines.toMap)._1
  }

}
