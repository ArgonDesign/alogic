package alogic

import scala.collection._

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

import Antlr4Conversions._
import alogic.antlr._
import alogic.antlr.VPreprocParser._
import scalax.file.Path

class Preproc(
    includeSearchPaths: List[Path],
    initialDefines: immutable.Map[String, String] = immutable.Map[String, String]()) {

  def this(
    includeSearchPaths: List[Path],
    initialDefines: mutable.Map[String, String]) = {
    this(includeSearchPaths, immutable.Map[String, String]() ++ initialDefines)
  }

  // Map of #define to substitution
  val defines = mutable.Map[String, String]() ++ initialDefines

  // Deferred line number remappings
  val remaps = mutable.Stack[(Range, Path)]()

  // Build the abstract syntax tree from a parse tree
  def apply(path: Path): String = {

    object PreprocVisitor extends VPreprocParserBaseVisitor[StrTree] {
      override def visitStart(ctx: StartContext) = visit(ctx.entities())

      override def visitEntities(ctx: EntitiesContext) = StrList(ctx.entity.toList.map(visit))

      override def visitHashDefine(ctx: HashDefineContext): StrTree = {
        val s = ctx.VIDENTIFIER.text
        if (defines contains s) {
          Message.warning(ctx.loc, s"Redefined preprocessor identifier '$s'")
        }
        defines(s) = ctx.VREST.text
        Str("")
      }

      override def visitIdentifier(ctx: IdentifierContext): StrTree = {
        val ident = ctx.IDENTIFIER.text
        Str(defines.getOrElse(ident, ident))
      }

      override def visitLiteral(ctx: LiteralContext): StrTree = Str(ctx.LITERAL.text)

      override def visitOneLineComment(ctx: OneLineCommentContext): StrTree = Str("\n")

      override def visitBlockComment(ctx: BlockCommentContext): StrTree = Str("\n" * ctx.text.count(_ == '\n'))

      override def visitAnything(ctx: AnythingContext): StrTree = Str(ctx.ANYTHING.text)

      override def visitHashIf(ctx: HashIfContext): StrTree = {
        val ident = ctx.IDENTIFIER.text
        if (defines contains ident) {
          val cond = defines(ident)
          val useElse = (cond.toInt == 0)
          val useFirst = !useElse
          val hasElse = ctx.entities.toList.length > 1

          val first = if (useFirst)
            visit(ctx.entities(0))
          else
            Str("\n" * ctx.entities(0).text.count(_ == '\n'))

          val second = if (useElse && hasElse)
            visit(ctx.entities(0))
          else if (hasElse)
            Str("\n" * ctx.entities(1).text.count(_ == '\n'))
          else
            Str("")

          StrList(first :: second :: Nil)
          // TODO catch exception if not an integer
          // TODO check it is either 0 or 1
        } else {
          Message.error(ctx.loc, s"Unknown preprocessor symbol $ident")
          Str("Unknown")
        }
      }

      override def visitHashInclude(ctx: HashIncludeContext): StrTree = {
        // Get the include path specifier
        val includeSpec = ctx.LITERAL.text.drop(1).dropRight(1)
        val includePath = Path.fromString(includeSpec)

        // Find the include file
        val resultPath: Path = if (includePath.isAbsolute) {
          Message.fatal(ctx, s"""No absolute include paths allowed: "$includeSpec"""")
        } else {
          // Prepend the directory of the including file to the search path
          val searchPaths = path.parent match {
            case Some(parent) => parent :: includeSearchPaths
            case None         => includeSearchPaths
          }

          // Look for the file
          searchPaths map (_ / includePath) find (_.exists) match {
            case Some(path) => path
            case None =>
              Message.fatal(ctx,
                s"""Cannot find include file "$includeSpec". Looked in:""" ::
                  (searchPaths map (path => s"""  "${path.path}"""")): _*)
          }
        }

        // Create a new preprocessor and process the include file
        val preproc = new Preproc(includeSearchPaths, defines)
        val text = preproc(resultPath)

        // Add the new #defines from the incldued file, there is no need
        // to warn for redefinitions here, as we have passed in 'defines'
        // to the nested preprocessor, so we have already yielded all warnings
        defines ++= preproc.defines

        // Record that we need to remap these locations for error messages,
        // but defer doing so until the whole file is processed. This ensures
        // errors printed during preprocessing are still printed correctly
        val start = ctx.loc.line
        val end = start + text.count(_ == '\n')
        remaps.push((start until end, resultPath))

        // Yield the preprocessed text
        Str(text)
      }
    }

    val text = path.lines(includeTerminator = true).mkString
    val inputStream = new ANTLRInputStream(text)
    inputStream.name = path.path

    val lexer = new antlr.VPreprocLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new antlr.VPreprocParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(ParserErrorListener)
    val parseTree = parser.start()

    val strTree = PreprocVisitor.visit(parseTree)

    // Update the source location map
    val remapList = remaps.toList.reverse
    val remapCumSum = remapList.map(_._1.size).scanLeft(0)(_ + _)
    for (((range, source), offset) <- remapList zip remapCumSum) {
      LocMap.remap(path, range.start + offset until range.end + offset, source)
    }

    MakeString(strTree)
  }
}
