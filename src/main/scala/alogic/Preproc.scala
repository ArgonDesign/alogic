////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import scala.collection._

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

import Antlr4Conversions._
import alogic.antlr._
import alogic.antlr.VPreprocParser._
import scalax.file.Path
import scala.util.Try

object Preproc {

  // The Cache maps
  // from (path, initialDefines)
  // to (text, finalDefines, finalRemaps)
  private object PreprocCache
    extends Cache[(Path, Map[String, String]), (String, Map[String, String], List[(Range, Path)])] {

    // Canonicalise path and convert to string to compute the unique tag
    override type Tag = (String, Map[String, String])

    override def index(key: Key): Tag = {
      val (path, initialDefines) = key;
      (path.toRealPath().path, initialDefines)
    }

  }

  // Private worker to use by recursive includes, returns defines as well
  private def process(
    source:          Source,
    includeResovler: (Source, String) => Either[List[String], Source],
    initialDefines:  immutable.Map[String, String]): (String, Map[String, String]) = {
    // Cache the text, final defines and the remapping
    val (text, defines, remaps) = PreprocCache((source.path, initialDefines)) {
      // Map of #define to substitution
      val defines = mutable.Map[String, String]() ++ initialDefines

      // Deferred line number remappings
      val remaps = mutable.Stack[(Range, Path)]()

      object PreprocVisitor extends VPreprocParserBaseVisitor[StrTree] {
        override def visitStart(ctx: StartContext) = visit(ctx.entities())

        override def visitEntities(ctx: EntitiesContext) = StrList(ctx.entity.toList.map(visit))

        override def visitHashDefine(ctx: HashDefineContext): StrTree = {
          val s = ctx.VIDENTIFIER.text
          if (defines contains s) {
            Message.warning(ctx.loc, s"Redefined preprocessor identifier '$s'")
          }
          defines(s) = ctx.VREST.text.trim
          Str("")
        }

        override def visitIdentifier(ctx: IdentifierContext): StrTree = {
          val ident = ctx.IDENTIFIER.text
          Str(defines.getOrElse(ident, ident))
        }

        override def visitLiteral(ctx: LiteralContext): StrTree = Str(ctx.LITERAL.text)

        override def visitLineComment(ctx: LineCommentContext): StrTree = Str(ctx.text)

        override def visitBlockComment(ctx: BlockCommentContext): StrTree = Str(ctx.text)

        override def visitAnything(ctx: AnythingContext): StrTree = Str(ctx.ANYTHING.text)

        override def visitHashIf(ctx: HashIfContext): StrTree = {
          val valueCond = ctx.ifcond.text == "if"
          val ident = ctx.IDENTIFIER.text

          val useElseOpt = if (valueCond) {
            if (defines contains ident) {
              val defineValue = defines(ident)
              Try(defineValue.toInt).toOption match {
                case Some(0) => Some(true)
                case Some(_) => Some(false)
                case None => {
                  Message.error(
                    ctx,
                    s"#if condition variabe '$ident' must be defined as a single integer,",
                    s"not '$defineValue'")
                  None
                }
              }
            } else {
              Message.error(ctx, s"#if condition variabe '$ident' is not defined")
              None
            }
          } else {
            Some(!(defines contains ident))
          }

          useElseOpt match {
            case Some(useElse) => {
              val hasElse = ctx.entities.toList.length > 1

              val first = if (!useElse)
                visit(ctx.entities(0))
              else
                Str("\n" * ctx.entities(0).text.count(_ == '\n'))

              val second = if (useElse && hasElse)
                visit(ctx.entities(1))
              else if (hasElse)
                Str("\n" * ctx.entities(1).text.count(_ == '\n'))
              else
                Str("")

              StrList(first :: second :: Nil)
            }
            case None => Str("/* Omitted due to error while preprocessing */")
          }
        }

        override def visitHashInclude(ctx: HashIncludeContext): StrTree = {
          // Get the include path specifier
          val includeSpec = ctx.LITERAL.text.drop(1).dropRight(1)

          // Find the include file
          val includeSource = includeResovler(source, includeSpec) match {
            case Left(msgs)          => Message.fatal(ctx, msgs: _*)
            case Right(resultSource) => resultSource
          }

          // Process the include file in the current context
          val (text, newDefines) = Preproc.process(includeSource, includeResovler, immutable.Map() ++ defines)

          // Add the new #defines from the included file, there is no need
          // to warn for redefinitions here, as we have passed in 'defines'
          // to the nested preprocessor, so we have already yielded all warnings
          defines ++= newDefines

          // Record that we need to remap these locations for error messages,
          // but defer doing so until the whole file is processed. This ensures
          // errors printed during preprocessing are still printed correctly
          val start = ctx.loc.line
          val end = start + text.count(_ == '\n')
          remaps.push((start until end, includeSource.path))

          // Yield the preprocessed text
          Str(text)
        }
      }

      // Create ANTLR input stream
      val inputStream = new ANTLRInputStream(source.text)
      inputStream.name = source.path.path

      // Create the lexer
      val lexer = new antlr.VPreprocLexer(inputStream)
      val tokenStream = new CommonTokenStream(lexer)
      tokenStream.fill()

      // Create the parser
      val parser = new antlr.VPreprocParser(tokenStream)
      parser.removeErrorListeners()
      parser.addErrorListener(ParserErrorListener)

      // Parse the the file
      val parseTree = parser.start()

      // Walk parse tree to do the work
      val strTree = PreprocVisitor.visit(parseTree)

      // Return processed contents as a string
      (strTree.toString, Map[String, String]() ++ defines, remaps.toList.reverse)
    }

    // Apply the deferred remappings to the source location map for this processed file
    val remapCumSum = remaps.scanLeft(0)(_ + _._1.size)
    for (((range, src), offset) <- remaps zip remapCumSum) {
      Loc.remap(source.path, range.start + offset until range.end + offset, src)
    }

    (text, defines)
  }

  // Preprocess a source, given include resolver and initial defines
  def apply(
    src:             Source,
    initialDefines:  Map[String, String],
    includeResolver: (Source, String) => Either[List[String], Source]): String = {
    process(src, includeResolver, initialDefines.toMap)._1
  }

  // Preprocess a source, given search paths and initial defines
  def apply(
    src:                Source,
    initialDefines:     Map[String, String],
    includeSearchPaths: List[Path]): String = {

    // Include file resolver that looks up include files in the search paths, or the
    // directory of the including file
    def includeResolver(source: Source, includeSpec: String): Either[List[String], Source] = {
      val includePath = Path.fromString(includeSpec)
      // Find the include file
      if (includePath.isAbsolute) {
        Left(List(s"""No absolute include paths allowed: "$includeSpec""""))
      } else {
        // Prepend the directory of the including file to the search path
        val searchPaths = source.path.parent match {
          case Some(parent) => parent :: includeSearchPaths
          case None         => includeSearchPaths
        }

        // Look for the file
        searchPaths map (_ / includePath) find (_.exists) match {
          case Some(path) => Right(Source(path))
          case None => Left(s"""Cannot find include file "$includeSpec". Looked in:""" ::
            (searchPaths map (path => s"""  "${path.path}"""")))
        }
      }
    }

    process(src, includeResolver, initialDefines.toMap)._1
  }
}
