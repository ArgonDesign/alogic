// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import AstOps._
import scala.annotation.tailrec

import scala.language.implicitConversions

final class MakeVerilog {

  val id2decl = mutable.Map[String, Declaration]()

  val go = "go" // Name of the signal used to decide whether to clock registers

  def error(msg: String) {
    println(msg)
    System.exit(-1)
  }

  // Name of signal used to check that a port contains valid data
  def valid(s: String): String =
    s + "_valid"

  // Name of signal used to signal that a port contains ready data
  def ready(s: String): String =
    s + "_ready"

  // Name of signal used to signal that a port contains ready data
  def accept(s: String): String =
    s + "_accept"

  // Collect all the declarations
  def apply(tree: StateProgram, fname: String): Unit = VisitAST(tree) {
    case DeclarationStmt(d) =>
      id2decl(ExtractName(d)) = d; false
    case _ => true
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

    def add(s: String): Unit = blockingStatements = s :: blockingStatements

    def v(tree: AlogicAST): Boolean = tree match {
      case CombinatorialCaseStmt(value, _) =>
        VisitAST(value)(v); false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        VisitAST(cond)(v); false
      case ReadCall(name, _) => {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case InDeclaration(synctype, _, _) => {
            if (HasValid(synctype))
              add(s"$go = $go && ${valid(n)};")
            if (HasReady(synctype))
              add(ready(n) + " = 1'b1;")
          }
          case _ => error(s"$name cannot be read"); false // TODO check this earlier?
        }
        false // No need to recurse
      }
      case WriteCall(name, _) => {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case OutDeclaration(synctype, _, _) => {
            synctype match {
              case SyncReadyBubble() => add(s"$go = $go && !${valid(n)};\n")
              case SyncReady()       => add(s"$go = $go && (!${valid(n)} || !${ready(n)});\n")
              case SyncAccept()      => error("sync accept only supported as wire output type") // TODO check this earlier
              case WireSyncAccept()  => add(s"$go = $go && ${accept(n)};\n")
              case _                 =>
            }
            if (HasValid(synctype))
              add(s"${valid(n)} = 1'b1;\n")
          }
          case _ => error(s"$name cannot be written"); false // TODO check this earlier?
        }
        true // Recurse in case arguments use reads
      }
      case _ => true
    }
    VisitAST(tree)(v)
    return blockingStatements
  }

  // Called with an integer representing a state, returns the appropriate string
  def MakeState(state: Int): String = s"$state" // TODO add correct number of bits

  // This function defines how to write next values (D input on flip-flops)
  def nx(x: String): StrTree = StrList(List(x, Str("_d")))

  def AddStall(indent: Int, stalls: List[String], expr: StrTree): StrTree = {
    if (stalls.isEmpty)
      StrList(" " * indent :: expr :: Nil)
    else
      StrList(Str(" " * indent) :: Str("begin\n") ::
        stalls.map(x => Str(" " * (indent + 4) + x)) :::
        Str(" " * indent) :: expr ::
        Str(" " * indent) :: Str("end\n") :: Nil)
  }

  // Produce code to go into the case statements (we assume we have already had a "case(state) default: begin"
  // The top call should be with Function or VerilogFunction
  def CombStmt(indent: Int, tree: AlogicAST): StrTree = tree match {
    case Assign(lhs, "=", rhs)                       => AddStall(indent, StallExpr(lhs) ::: StallExpr(rhs), StrList(List(MakeExpr(lhs), "=", MakeExpr(rhs))))
    case CombinatorialCaseStmt(value, cases)         => Str("TODO")
    case CombinatorialIf(cond, body, elsebody)       => Str("TODO")
    case WriteCall(name, args) if (args.length == 1) => AddStall(indent, StallExpr(name) ::: StallExpr(args(0)), StrList(List(MakeExpr(name), "=", MakeExpr(args(0)))))
    case CombinatorialBlock(cmds) => StrList(Str(" " * indent) :: Str("begin\n") ::
      StrList(for { cmd <- cmds } yield CombStmt(indent + 4, cmd)) ::
      Str(" " * indent) :: Str("end\n") :: Nil)
    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => CombStmt(indent, Assign(id, "=", rhs))
    case Plusplus(lhs) => CombStmt(indent, Assign(lhs, "=", BinaryOp(lhs, "+", Num("1'b1"))))
    case AlogicComment(s) => s"// $s\n"
    case StateStmt(state) => StrList(List(" " * (indent - 4), "end\n", " " * (indent - 4), MakeState(state), ": begin\n"))
    case GotoState(target) => StrList(List(" " * indent, nx("state"), "=", MakeState(target), ";\n"))
    case x => error("Don't know how to emit code for $x"); Str("")
  }
}
