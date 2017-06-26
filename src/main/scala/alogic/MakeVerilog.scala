// This class constructs a Verilog file from an AST

package alogic

import scala.collection._
import java.io._

import AstOps._
import scala.annotation.tailrec

import scala.language.implicitConversions

final class MakeVerilog {

  val id2decl = mutable.Map[String, Declaration]()

  val go = "go" // Name of the signal used to decide whether to clock registers

  var numstates = 0

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

  def apply(tree: StateProgram, fname: String): Unit = {
    numstates = tree.numStates
    // Collect all the declarations
    VisitAST(tree) {
      case Task(_, _, decls, _) => { decls foreach { x => id2decl(ExtractName(x)) }; false }
      case DeclarationStmt(d)   => { id2decl(ExtractName(d)) = d; false }
      case _                    => true
    }

    // Emit header and combinatorial code
    var fns: List[StrTree] = Nil // Collection of function code
    var fencefns: List[StrTree] = Nil // Collection of fence function code
    var clears: List[StrTree] = Nil // Collection of outputs to clear if !go
    var defaults: List[StrTree] = Nil // Collection of things to set at start of each cycle
    var clocks: List[StrTree] = Nil // Collection of things to clock if go
    var resets: List[StrTree] = Nil // Collection of things to reset

    val pw = new PrintWriter(new File(fname))

    def writeSigned(signed: Boolean) = if (signed) "signed " else ""

    def writeOut(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, 1)    => pw.println(s"out ${writeSigned(b)}reg " + MakeString(name) + ";")
      case IntType(b, size) => pw.println(s"out ${writeSigned(b)}reg [$size-1:0]" + MakeString(name) + ";")
      case _                => // TODO
    }
    def writeIn(typ: AlogicType, name: StrTree): Unit = typ match {
      case IntType(b, 1)    => pw.println(s"in ${writeSigned(b)}wire " + MakeString(name) + ";")
      case IntType(b, size) => pw.println(s"in ${writeSigned(b)}wire [$size-1:0]" + MakeString(name) + ";")
      case _                => // TODO
    }
    def writeVar(typ: AlogicType, name: StrTree): Unit = {
      typ match {
        case IntType(b, 1)    => pw.println(s"${writeSigned(b)}reg " + MakeString(name) + ";")
        case IntType(b, size) => pw.println(s"${writeSigned(b)}reg [$size-1:0]" + MakeString(name) + ";")
        case _                => // TODO variable int type
      }
      typ match {
        case IntType(a, num) => {
          resets = StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= $num'b0;\n") :: Nil) :: resets
          clocks = StrList(Str("      ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil) :: resets
        }
        case _ =>
      }
    }

    VisitAST(tree) {
      case Task(Fsm(), name, decls, fns) => {
        pw.println(s"module $name (")
        // TODO params
        id2decl.values.foreach({
          case OutDeclaration(synctype, decltype, name) => {
            if (HasValid(synctype)) {
              pw.println("out reg " + valid(name) + ";")
              clears = Str("    " + valid(name) + " = 1'b0;\n") :: clears
              defaults = Str("    " + valid(name) + " = 1'b0;\n") :: defaults
            }
            if (HasReady(synctype))
              pw.println("in wire " + ready(name) + ";")
            if (HasAccept(synctype))
              pw.println("out reg " + accept(name) + ";")
            VisitType(decltype, name)(writeOut)

          }
          case InDeclaration(synctype, decltype, name) => {
            if (HasValid(synctype))
              pw.println("in wire " + valid(name) + ";")
            if (HasReady(synctype)) {
              pw.println("out reg " + ready(name) + ";")
              clears = Str("    " + ready(name) + " = 1'b0;\n") :: clears
              defaults = Str("    " + ready(name) + " = 1'b0;\n") :: defaults
            }
            if (HasAccept(synctype))
              pw.println("in wire " + accept(name) + ";")
            VisitType(decltype, name)(writeOut)
          }
          case _ =>
        })
        pw.println(") begin")
        // declare remaining variables
        id2decl.values.foreach({
          case VarDeclaration(decltype, name, init) => VisitType(decltype, ExtractName(name))(writeVar)
          case _                                    =>
        })
        true
      }
      case Function(name, body) => { fns = CombStmt(6, body) :: fns; false }
      case FenceFunction(body)  => { fencefns = CombStmt(4, body) :: fencefns; false }
      case _                    => true
    }
    // Start main combinatorial loop
    pw.println("always @* begin")
    pw.println("  go = 1'b1;")
    // Prepare defaults
    pw.println(MakeString(StrList(defaults)))
    pw.println(MakeString(StrList(fencefns)))
    pw.println("  case(state) begin")
    pw.println("    default: begin")
    pw.println(MakeString(StrList(fns)))
    pw.println("    end")
    pw.println("  end")
    if (clears.length > 0) {
      pw.println(s"  if (!$go) begin")
      pw.write(MakeString(clears))
      pw.println("  end")
    }
    pw.println("end")
    // Now emit clocked blocks
    pw.println("always @(posedge clk or negedge rst_n) begin")
    pw.println("  if (!rst_n) begin")
    pw.println(MakeString(StrList(resets)))
    pw.println("  end else begin")
    pw.println(s"    if ($go) begin")
    pw.println(MakeString(StrList(clocks)))
    pw.println("    end")
    pw.println("  end")
    pw.println("end")
    pw.println("endmodule")
    pw.close()
  }

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(names: List[String]): String = names.mkString("_") // TODO inspect type and expand
  def nx(name: StrTree): String = MakeString(name) + "_d"

  // Construct the string to be used when this identifier is used on the LHS of an assignment
  def reg(names: List[String]): String = names.mkString // TODO inspect type and expand
  def reg(name: StrTree): String = MakeString(name) + "_q"

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

    def AddRead(name: DottedName, isLock: Boolean): Boolean = {
      val n: String = ExtractName(name)
      val d: Declaration = id2decl(n)
      d match {
        case InDeclaration(synctype, _, _) => {
          if (HasValid(synctype))
            add(s"$go = $go && ${valid(n)};")
          if (!isLock && HasReady(synctype))
            add(ready(n) + " = 1'b1;")
        }
        case _ => error(s"$name cannot be read"); false // TODO check this earlier?
      }
      false // No need to recurse
    }

    def v(tree: AlogicAST): Boolean = tree match {
      case CombinatorialCaseStmt(value, _) =>
        VisitAST(value)(v); false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        VisitAST(cond)(v); false
      case ReadCall(name, _)   => AddRead(name, false)
      case LockCall(name, _)   => AddRead(name, true)
      case UnlockCall(name, _) => AddRead(name, false)
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
  def MakeState(state: Int): String = s"$numstates'd$state"

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
    case Assign(lhs, "=", rhs) => AddStall(indent, StallExpr(lhs) ::: StallExpr(rhs), StrList(List(MakeExpr(lhs), "=", MakeExpr(rhs))))
    case CombinatorialCaseStmt(value, cases) => AddStall(indent, StallExpr(value),
      StrList(Str(" " * indent) :: Str("case(") :: MakeExpr(value) :: Str(") begin\n") ::
        StrList(for (c <- cases) yield CombStmt(indent + 4, c)) ::
        Str(" " * indent) :: Str("end\n") :: Nil))
    case CombinatorialIf(cond, body, Some(elsebody)) => AddStall(indent, StallExpr(cond),
      StrList(Str(" " * indent) :: Str("if\n") ::
        CombStmt(indent + 4, body) ::
        Str(" " * indent) :: Str("else\n") ::
        CombStmt(indent + 4, elsebody) :: Nil))
    case CombinatorialIf(cond, body, None) => AddStall(indent, StallExpr(cond),
      StrList(Str(" " * indent) :: Str("if\n") ::
        CombStmt(indent + 4, body) :: Nil))
    case LockCall(_, _)                              => Str("")
    case UnlockCall(_, _)                            => Str("")
    case WriteCall(name, args) if (args.length == 1) => AddStall(indent, StallExpr(name) ::: StallExpr(args(0)), StrList(List(MakeExpr(name), "=", MakeExpr(args(0)))))
    case CombinatorialBlock(cmds) => StrList(Str(" " * indent) :: Str("begin\n") ::
      StrList(for { cmd <- cmds } yield CombStmt(indent + 4, cmd)) ::
      Str(" " * indent) :: Str("end\n") :: Nil)
    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => CombStmt(indent, Assign(id, "=", rhs))
    case Plusplus(lhs) => CombStmt(indent, Assign(lhs, "=", BinaryOp(lhs, "+", Num("1'b1"))))
    case AlogicComment(s) => s"// $s\n"
    case StateStmt(state) => StrList(List(" " * (indent - 4), "end\n", " " * (indent - 4), MakeState(state), ": begin\n"))
    case GotoState(target) => StrList(List(" " * indent, nx("state"), "=", MakeState(target), ";\n"))
    case CombinatorialCaseLabel(conds, body) => StrList(
      Str(" " * indent) ::
        StrCommaList(conds.map(MakeExpr)) ::
        Str(":\n") :: CombStmt(indent + 4, body) :: Nil)
    case x => error("Don't know how to emit code for $x"); Str("")
  }
}
