// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

import scala.language.implicitConversions

final class MakeVerilog {

  val id2decl = mutable.Map[String, Declaration]()

  def error(msg: String) {
    println(msg)
    System.exit(-1)
  }

  // Collect all the declarations
  def apply(tree: StateProgram, fname: String): Unit = VisitAST(tree) {
    case DeclarationStmt(d) => id2decl(ExtractName(d)) = d
    case _                  =>
  }

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(names: List[String]): String = names.mkString("_") // TODO

  implicit def string2StrTree(s: String): StrTree = Str(s)
  implicit def stringList2StrTree(s: List[StrTree]): StrTree = StrList(s)

  // Construct a string for an expression
  def MakeExpr(tree: AlogicAST): StrTree = {
    tree match {
      case ArrayLookup(name, index)              => StrList(List(MakeExpr(name), "[", MakeExpr(index), "]"))
      case BinaryArrayLookup(name, lhs, op, rhs) => StrList(List(MakeExpr(name), "[", MakeExpr(lhs), op, MakeExpr(rhs), "]"))
      case FunCall(name, args)                   => StrList(List(MakeExpr(name), "(", StrCommaList(args.map(MakeExpr)), ")"))
      // TODO      case Zxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
      // TODO     case Sxt(numbits, expr) => {VisitAST(numbits,callback); VisitAST(expr,callback)}
      case DollarCall(name, args)                => StrList(List(name, "(", StrCommaList(args.map(MakeExpr)), ")"))
      case ReadCall(name, args)                  => MakeExpr(name)
      case BinaryOp(lhs, op, rhs)                => StrList(List(MakeExpr(lhs), op, MakeExpr(rhs)))
      case UnaryOp(op, lhs)                      => StrList(List(op, MakeExpr(lhs)))
      case Bracket(content)                      => StrList(List("(", MakeExpr(content), ")"))
      case TernaryOp(cond, lhs, rhs)             => StrList(List(MakeExpr(cond), "?", MakeExpr(lhs), ":", MakeExpr(rhs)))
      case BitRep(count, value)                  => StrList(List("{", MakeExpr(count), "{", MakeExpr(value), "}}"))
      case BitCat(parts)                         => StrList(List("{", StrCommaList(parts.map(MakeExpr)), "}"))
      case DottedName(names)                     => nx(names)
      case Literal(s)                            => StrList(List(""""""", s, """""""))
      case Num(n)                                => n
      case e                                     => error(s"Unexpected expression $e"); ""
    }
  }

  // Take a combinatorial statement and return stall conditions that should be emitted now based on ids that are read/written
  // This code will be emitted before the actual statement
  // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
  // Strings have \n at the end, but indent will be added later
  def StallExpr(tree: AlogicAST): List[String] = {
    // Use a local function to avoid having to copy emitted list down the stack
    var blockingStatements: List[String] = Nil
    def go(tree: AlogicAST): Unit = {

    }
    return blockingStatements
  }
}
