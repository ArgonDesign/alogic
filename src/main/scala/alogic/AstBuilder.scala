package alogic

import alogic.antlr4._
import alogic.antlr4.VParser._
import scala.collection.JavaConverters._
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.ParserRuleContext
import scala.collection._
import scala.collection.mutable.ListBuffer

// The aim of the AstBuilder stage is:
//   Build an abstract syntax tree
//   Deal with as many error conditions as possible (while we can easily report error location)
//   Deal with typedefs
//   Deal with variable scope
//   Deal with #defines
//
// We use different visitors for the different things we wish to extract.
//
// The overall API is to make a AstBuilder class, and then call it to build the ast from a parse tree.
// The object can be called multiple times to allow us to only build a common header file once.

class AstBuilder {

  val typedefs = mutable.Map[String,AlogicType]()
  val defines = mutable.Map[String,AlogicAST]()
  
  val errors = new ListBuffer[String]()
  
  def warning(ctx: ParserRuleContext, msg: String) {
    val tok = ctx.getStart()
    val line = tok.getLine()
    val pos = tok.getCharPositionInLine()
    errors += s"line $line:$pos $msg"
  }

  object ProgVisitor extends VParserBaseVisitor[List[AlogicAST]] {
    override def visitStart(ctx: StartContext) = {
      ctx.entities.asScala.toList.map(EntityVisitor.visit)
    }
  }
  
  object EntityVisitor extends VParserBaseVisitor[AlogicAST] {
    override def visitTypedef(ctx: TypedefContext) = {
      val s = ctx.IDENTIFIER().getText()
      if (typedefs contains s) {
        warning(ctx,s"Repeated typedef $s")
      } else {
        typedefs(s) = TypeVisitor.visit(ctx.known_type())
      }
      Typedef()
    }
    
    override def visitDefine(ctx: DefineContext) = {
      val s = ctx.IDENTIFIER().getText()
      if (defines contains s) {
        warning(ctx,s"Repeated identifier $s")
      } else {
        defines(s) = Define() // TODO
      }
      Define()
    }
    
    override def visitTask(ctx: TaskContext) = {
      Name("task")
      Task()
    }
  }
  
  object TypeVisitor extends VParserBaseVisitor[AlogicType] {
    override def visitBoolType(ctx: BoolTypeContext) = IntType(false,1)
    
    override def visitIntType(ctx: IntTypeContext) = {
      val s = ctx.INTTYPE().getText()
      val n = s.substring(1,s.length)
      IntType(true,n.toInt)
    }
    
    override def visitUintType(ctx: UintTypeContext) = {
      val s = ctx.UINTTYPE().getText()
      val n = s.substring(1,s.length)
      IntType(false,n.toInt)
    }
    
    override def visitIdentifierType(ctx: IdentifierTypeContext) = {
      val s = ctx.IDENTIFIER().getText()
      typedefs.getOrElse( s, {
        warning(ctx,s"Unknown type $s")
        IntType(false,1)
      } )
    }
    
    // TODO IntVType and UintVType and Struct
    
  }
  
  // Return if this node is a task node
  def is_task(ast:AlogicAST) : Boolean = ast match {case Task() => true; case _ => false}
  
  // Build the abstract syntax tree from a parse tree
  def apply(parseTree : ParseTree) : Program = {
    val p = Program(ProgVisitor.visit(parseTree).filter(is_task))
    errors.foreach(println)
    p
  }
}

