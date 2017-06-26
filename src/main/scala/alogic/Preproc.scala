package alogic

import scala.collection._
import scala.collection.mutable.ListBuffer

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTree

import Antlr4Conversions._
import alogic.antlr._
import alogic.antlr.VPreprocParser._

class Preproc {

  val defines = mutable.Map[String, String]()
  val errors = new ListBuffer[String]()

  def warning(ctx: ParserRuleContext, msg: String) {
    errors += s"line ${ctx.loc} $msg"
  }

  // Add definitions from another file
  def add(old: Preproc) {
    defines ++= old.defines
  }

  // Convert identifier to tree

  object PreprocVisitor extends VPreprocParserBaseVisitor[StrTree] {
    override def visitStart(ctx: StartContext) = visit(ctx.entities())

    override def visitEntities(ctx: EntitiesContext) = StrList(ctx.entity.toList.map(visit))

    override def visitHashDefine(ctx: HashDefineContext): StrTree = {
      val s = ctx.VIDENTIFIER.text
      if (defines contains s) {
        warning(ctx, s"Repeated identifier $s")
      }
      defines(s) = ctx.VREST.text
      Str("")
    }

    override def visitIdentifier(ctx: IdentifierContext): StrTree = {
      val ident = ctx.IDENTIFIER.text
      Str(defines.getOrElse(ident, ident))
    }

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
        warning(ctx, s"Unknown preprocessor symbol $ident")
        Str("Unknown")
      }
    }
  }

  // Build the abstract syntax tree from a parse tree
  def apply(parseTree: ParseTree): String = {
    val p = MakeString(PreprocVisitor.visit(parseTree))
    errors.foreach(println)
    p
  }
}
