// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

final class MakeVerilog {

  val id2decl = mutable.Map[String,Declaration]()
  
  def error(msg: String) {
    println(msg)
    System.exit(-1) 
  }

  def apply(tree:StateProgram, fname: String) : Unit = {
    // Collect all the declarations
    VisitAST( tree, {
      case DeclarationStmt(d) => id2decl(ExtractName(d)) = d
      case _ => 
    })
  }
  
  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(names: List[String]): String = names.mkString("_") // TODO
  
  // Construct a string for an expression
  def MakeExpr(tree: AlogicAST): String = {
    tree match {
      case ArrayLookup(name, index) => s"${MakeExpr(name)} [ ${MakeExpr(index)} ]"
      case BinaryArrayLookup(name, lhs, op, rhs) => s"${MakeExpr(name)} [ ${MakeExpr(lhs)} ${op} ${MakeExpr(rhs)} ]"
      case FunCall(name, args) => s"${MakeExpr(name)} ( ${args.map(MakeExpr).mkString} )"
// TODO      case Zxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
// TODO     case Sxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
      case DollarCall(name, args) => s"$name ( ${args.map(MakeExpr).mkString(",")} )"
      case ReadCall(name, args) => MakeExpr(name)
      case BinaryOp(lhs, op, rhs) => s"${MakeExpr(lhs)} $op ${MakeExpr(rhs)}"
      case UnaryOp(op, lhs) => s"$op ${MakeExpr(lhs)}"
      case Bracket(content) => s"( ${MakeExpr(content)} )"
      case TernaryOp(cond, lhs, rhs) => s"${MakeExpr(cond)} ? ${MakeExpr(lhs)} : ${MakeExpr(rhs)}"
      case BitRep(count,value)  => s"{${MakeExpr(count)}{${MakeExpr(value)}}"
      case BitCat(parts) => s"{ ${parts.map(MakeExpr).mkString(",")} }"
      case DottedName(names) => nx(names)
      case Literal(s) => s
      case Num(n) => n
      case e => error(s"Unexpected expression $e"); ""
    }
  }  

}


