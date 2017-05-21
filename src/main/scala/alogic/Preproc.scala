package alogic

import alogic.antlr._
import alogic.antlr.VPreprocParser._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.ParserRuleContext
import scala.collection._
import scala.collection.mutable.ListBuffer
import alogic.AstOps._

// TODO strip comments here as well
// (Avoids issues with #defines in comments!)

class Preproc {

  val defines = mutable.Map[String, String]()
  val errors = new ListBuffer[String]()

  def warning(ctx: ParserRuleContext, msg: String) {
    val tok = ctx.getStart()
    val line = tok.getLine()
    val pos = tok.getCharPositionInLine()
    errors += s"line $line:$pos $msg"
  }

  // Add definitions from another file
  def add(old: Preproc) {
    defines ++= old.defines
  }

  // Convert identifier to tree

  object PreprocVisitor extends VPreprocParserBaseVisitor[StrTree] {
    override def visitStart(ctx: StartContext) = visit(ctx.entities())

    override def visitEntities(ctx: EntitiesContext) = StrList(ctx.entity().asScala.toList.map(visit))

    override def visitHashDefine(ctx: HashDefineContext): StrTree = {
      val s = ctx.VIDENTIFIER().getText()
      if (defines contains s) {
        warning(ctx, s"Repeated identifier $s")
      }
      defines(s) = ctx.VREST().getText()
      Str("")
    }

    override def visitIdentifier(ctx: IdentifierContext): StrTree = {
      val ident = ctx.IDENTIFIER().getText()
      Str(defines.getOrElse(ident, ident))
    }

    override def visitOneLineComment(ctx: OneLineCommentContext): StrTree = Str("\n")

    override def visitBlockComment(ctx: BlockCommentContext): StrTree = Str("\n" * ctx.getText().count(_ == '\n'))

    override def visitAnything(ctx: AnythingContext): StrTree = Str(ctx.ANYTHING().getText())

    override def visitHashIf(ctx: HashIfContext): StrTree = {
      val ident = ctx.IDENTIFIER().getText()
      if (defines contains ident) {
        val cond = defines(ident)
        if (cond.toInt == 0)
          visit(ctx.entities(1))
        else
          visit(ctx.entities(0))
        // TODO catch exception if not an integer
        // TODO check it is either 0 or 1
        // TODO replace with appropriate number of blank lines to make line numbers match up
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

