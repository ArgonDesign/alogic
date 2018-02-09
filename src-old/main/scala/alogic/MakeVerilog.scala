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

// This class constructs a Verilog file from an AST

package alogic

import java.io._

import scala.collection._
import scala.collection.mutable.Stack
import scala.language.implicitConversions

import alogic.ast._
import alogic.ast.AstOps._
import scalax.file.Path

final class MakeVerilog(moduleCatalogue: Map[String, Task])(implicit cc: CompilerContext) {

  val i0 = "  "; // Single indentation depth (2 spaces)

  val id2decl = mutable.Map[String, Decl]()

  val nxMap = mutable.Map[String, String]() // Returns string to use when this identifier is accessed
  val regMap = mutable.Map[String, String]() // Map from name in alogic to name in Verilog

  val Arrays = mutable.Set[String]()

  val go = "go" // Name of the signal used to decide whether to clock registers

  var numstates = 0

  var log2numstates = 0

  // Name of signal used to check that a port contains valid data
  def valid(s: String): String =
    s + "_valid"

  // Name of signal used to signal that a port contains ready data
  def ready(s: String): String =
    s + "_ready"

  // Name of signal used to signal that a port contains ready data
  def accept(s: String): String =
    s + "_accept"

  // Compute number of address bits required for a given depth
  def ceillog2(x: Int): Int = {
    var y = 0
    while ((1 << y) < x)
      y += 1
    y
  }

  var makingAccept = false // We use this to decide how to emit expressions

  def writeEntityHeader(entity: Task, pw: PrintWriter): Unit = {
    val Task(_, modname, decls) = entity

    val outtype = entity match {
      case _: StateTask   => "reg "
      case _: NetworkTask => "wire "
      case _: VerilogTask => "reg "
      case _: FsmTask     => unreachable
    }

    pw.println(s"`default_nettype none")
    pw.println()

    pw.print(s"module $modname ")

    val paramDecls = decls collect { case x: DeclParam => x }

    if (paramDecls.isEmpty) {
      pw.println(s"(")
    } else {
      pw.println(s"#(")
      // Emit parameter declarations
      val s = for (DeclParam(kind, id, init) <- paramDecls) yield {
        s"${i0}parameter " + Signal(id, kind).declString + "=" + MakeExpr(init)
      }
      pw.println(s mkString ",\n")
      pw.println(s") (")
    }

    def writeOut(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType       => pw.println("  output " + outtype + Signal(name.toString, kind).declString + ",")
      case (_: Struct | VoidType) => /* Nothing to do */
    }
    def writeIn(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType       => pw.println("  input wire " + Signal(name.toString, kind).declString + ",")
      case (_: Struct | VoidType) => /* Nothing to do */
    }

    // Emit port declarations
    decls foreach {
      case DeclOut(kind, name, fctype, stype) => {
        if (fctype.hasValid) pw.println("  output " + outtype + valid(name) + ",")
        if (fctype.hasReady) pw.println("  input wire " + ready(name) + ",")
        if (fctype.hasAccept) pw.println("  input wire " + accept(name) + ",")
        VisitType(kind, name)(writeOut)
      }
      case DeclIn(kind, name, fctype) => {
        if (fctype.hasValid) pw.println("  input wire " + valid(name) + ",")
        if (fctype.hasReady) pw.println("  output " + outtype + ready(name) + ",")
        if (fctype.hasAccept) pw.println("  output " + outtype + accept(name) + ",")
        VisitType(kind, name)(writeIn)
      }
      case _ =>
    }

    //Goes on bottom so we don't need to special case the lack of comma after the last port
    pw.println("  input wire clk,")
    pw.println("  input wire rst_n")
    pw.println(");\n")

    // Emit localparam (const) declaratoins
    decls foreach {
      case DeclConst(kind, id, init) => {
        pw.println(s"${i0}localparam " + Signal(id, kind).declString + "=" + MakeExpr(init) + ";")
      }
      case _ =>
    }
  }

  def apply(task: Task, opath: Path): Unit = {
    val Task(_, modname, decls) = task

    // Collect all the declarations
    decls foreach { x => id2decl(x.id) = x }
    task visit {
      // We remove the initializer from Decl statements
      // These will be reset inline where they are declared.
      case DeclarationStmt(_, DeclVar(kind, id, _)) => { id2decl(id) = DeclVar(kind, id, None); false }
      case _                                        => true
    }

    // Prepare the mapping for the nx() map
    // Different types of variables need different suffices
    // For efficiency this returns the comma separated list of underlying fields
    // TODO better for Verilator simulation speed if we could detect assignments and expand this concatenation
    def SetNxType(m: mutable.Map[String, String], typ: Type, name: String, suffix: String): String = typ match {
      case Struct(_, fields) => {
        val c = for ((n, t) <- fields) yield {
          SetNxType(m, t, name + "_" + n, suffix)
        }
        val commaFields = c.mkString(",")
        m(name) = "{" + commaFields + "}"
        commaFields
      }
      case _ => {
        val t = name + suffix
        m(name) = t
        t
      }
    }

    id2decl.values foreach {
      case DeclIn(kind, name, _) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case DeclOut(kind, name, fctype, stype) => {
        stype match {
          case StorageTypeWire => SetNxType(nxMap, kind, name, "")
          case _               => SetNxType(nxMap, kind, name, "_nxt")
        }
        SetNxType(regMap, kind, name, "")
        if (fctype.hasValid) {
          SetNxType(regMap, IntType(false, 1), valid(name), "")
          if (stype == StorageTypeWire)
            SetNxType(nxMap, IntType(false, 1), valid(name), "")
          else
            SetNxType(nxMap, IntType(false, 1), valid(name), "_nxt")
        }
      }
      case DeclParam(kind, name, init) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case DeclConst(kind, name, init) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case DeclVar(kind, name, _) => {
        SetNxType(nxMap, kind, name, "_nxt")
        SetNxType(regMap, kind, name, "")
      }
      case DeclArr(kind, name, _) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case DeclVerilogVar(kind, name) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case DeclVerilogArr(kind, name, _) => {
        SetNxType(nxMap, kind, name, "")
        SetNxType(regMap, kind, name, "")
      }
      case x => cc.ice(s"Don't know how to handle ${x}")
    }

    // Create output file
    opath.createFile(createParents = true, failIfExists = false)
    val pw = new PrintWriter(new File(opath.path))

    ///////////////////////////////////////////////////////////////////////////
    // Write entity header
    ///////////////////////////////////////////////////////////////////////////
    writeEntityHeader(task, pw)

    ///////////////////////////////////////////////////////////////////////////
    // Write entity body
    ///////////////////////////////////////////////////////////////////////////
    task match {
      case network: NetworkTask       => finishNetwork(network, pw)
      case VerilogTask(_, _, _, vfns) => vfns foreach { vfn => pw.print(vfn.body) }
      case statetask: StateTask       => finishState(statetask, pw)
      case _                          => unreachable
    }

    ///////////////////////////////////////////////////////////////////////////
    // Write entity footer
    ///////////////////////////////////////////////////////////////////////////
    pw.println("endmodule")
    pw.println()
    pw.println(s"`default_nettype wire")
    pw.close()
  }

  def finishNetwork(network: NetworkTask, pw: PrintWriter): Unit = {
    assert(network.fsms.isEmpty)

    // Collect all instatiations
    val optInstances = for (Instantiate(attr, id, module, args) <- network.instantiate) yield {
      if (!(moduleCatalogue contains module)) {
        cc.error(s"Cannot instantiate undefined module '${module}' in module '${network.name}'")
        None
      } else {
        Some(new ModuleInstance(attr, id, moduleCatalogue(module), args))
      }
    }

    if (optInstances contains None) {
      return
    }

    val instances = optInstances.flatten

    // Construct an instance name -> ModuleInstance map
    val modMap = {
      val pairs = instances map { instance => instance.name -> instance }
      immutable.Map("this" -> new ModuleInstance(network.attr, "this", network, Map())) ++ pairs
    }

    // Collect port connections
    val portConnections = network.connect flatMap {
      case Connect(_, DottedName(a, fromName :: fromPortName :: Nil), to) => {
        val fromInstance = modMap.getOrElse(fromName, cc.fatal(a, s"Unknown module name '${fromName}'"))
        val fromPort = if (fromName == "this") fromInstance.iwires(fromPortName) else fromInstance.owires(fromPortName)
        to flatMap {
          case DottedName(a, toName :: toPortName :: Nil) => {
            val toInstance = modMap.getOrElse(toName, cc.fatal(a, s"Unknown module name '${toName}'"))
            val toPortOpt = if (toName == "this") toInstance.owires.get(toPortName) else toInstance.iwires.get(toPortName)

            toPortOpt match {
              case None => {
                cc.error(a, s"No port named '${toPortName}' on instance '${toName}' (module '${toInstance.task.name}')")
                None
              }
              case Some(toPort) => {
                lazy val msg = s"Flow control of port '${fromName}.${fromPortName}' is not compatible with port '${toName}.${toPortName}'"
                (fromPort, toPort) match {
                  case (_: PortNone, _: PortNone)     => // OK
                  case (_: PortNone, _: PortValid)    => cc.error(a, msg, "none -> sync")
                  case (_: PortNone, _: PortReady)    => cc.error(a, msg, "none -> sync ready")
                  case (_: PortNone, _: PortAccept)   => cc.error(a, msg, "none -> sync accept")

                  case (_: PortValid, _: PortNone)    => cc.error(a, msg, "sync -> none")
                  case (_: PortValid, _: PortValid)   => // OK
                  case (_: PortValid, _: PortReady)   => cc.error(a, msg, "sync -> sync ready")
                  case (_: PortValid, _: PortAccept)  => cc.error(a, msg, "sync -> sync accept")

                  case (_: PortReady, _: PortNone)    => cc.error(a, msg, "sync ready -> none")
                  case (_: PortReady, _: PortValid)   => cc.error(a, msg, "sync ready -> sync")
                  case (_: PortReady, _: PortReady)   => // OK
                  case (_: PortReady, _: PortAccept)  => // OK (accept can drive ready)

                  case (_: PortAccept, _: PortNone)   => cc.error(a, msg, "sync accept -> none")
                  case (_: PortAccept, _: PortValid)  => cc.error(a, msg, "sync accept -> sync")
                  case (_: PortAccept, _: PortReady)  => cc.error(a, msg, "sync accept -> sync ready")
                  case (_: PortAccept, _: PortAccept) => // OK
                }

                // TODO: check ports have compatible type
                Some((fromInstance, fromPort, toInstance, toPort))
              }
            }
          }
        }
      }
    }

    // Emit all verilog functions
    network.vfns foreach { vfn =>
      pw.print(vfn.body)
    }

    // Emit instantiations
    for (instance <- instances) {
      val paramAssigns = instance.paramAssigns
      val paramValues = instance.task.defaultParams ++ paramAssigns
      // declare all port wires
      pw.println(s"${i0}// Port wires for ${instance.name}")
      for (port <- instance.wires; Signal(name, kind) <- port.signals) {
        // substitute formal parameters with actual parameters
        // TODO: This still cannot cope with dependent default parameters
        val actualWidth = kind.widthExpr rewrite {
          case DottedName(_, name :: Nil) if (paramValues contains name) => paramValues(name)
        }
        val signal = Signal(name, IntVType(kind.signed, actualWidth :: Nil))
        pw.println(s"${i0}wire ${signal.declString};")
      }
      pw.println()

      // instantiate
      if (paramAssigns.nonEmpty) {
        val pas = paramAssigns map {
          case (lhs, rhs) => s".${lhs}(${rhs.toVerilog})"
        }
        pw.println(s"""|${i0}${instance.task.name} #(
                       |${i0 * 2}${pas mkString s",\n${i0 * 2}"}
                       |${i0})""".stripMargin)
      } else {
        pw.println(s"${i0}${instance.task.name}")
      }

      val portAssigns = for {
        (port, wire) <- instance.ports zip instance.wires
        (Signal(portName, _), Signal(wireName, _)) <- port.signals zip wire.signals
      } yield {
        s".${portName}(${wireName})"
      }
      val miscAssigns = ".clk(clk)" :: ".rst_n(rst_n)" :: Nil
      pw.println(s"""|${i0}${instance.name} (
                     |${i0 * 2}${miscAssigns ::: portAssigns mkString s",\n${i0 * 2}"}
                     |${i0});""".stripMargin)
      pw.println()
    }

    // Assign all ports
    for ((srcInstance, srcPort, dstInstance, dstPort) <- portConnections) {
      // Connect payload signals
      val dstSignals = dstPort.payload map { _.name }
      val srcSignals = srcPort.payload map { _.name }
      pw.println(s"${i0}// ${srcPort.name} -> ${dstPort.name}")
      dstSignals match {
        case Nil        => () // Void port
        case sig :: Nil => pw.print(s"${i0}assign ${sig} = ") // Scalar port
        case sigs => pw.print(s"""|${i0}assign {
                                  |${i0 * 2}${sigs mkString s",\n${i0 * 2}"}
                                  |${i0}} = """.stripMargin)
      }
      srcSignals match {
        case Nil        => () // Void port
        case sig :: Nil => pw.println(s"${sig};") // Scalar port
        case sigs => pw.println(s"""|{
                                    |${i0 * 2}${sigs mkString s",\n${i0 * 2}"}
                                    |${i0}};""".stripMargin)
      }

      // Connect flow control signals
      // We already checked that the connected ports are compatible, so no need to check here
      (dstPort.valid, srcPort.valid) match {
        case (Some(dstSig), Some(srcSig)) => pw.println(s"${i0}assign ${dstSig.name} = ${srcSig.name};")
        case _                            =>
      }
      (dstPort.ready, srcPort.ready) match {
        case (Some(dstSig), Some(srcSig)) => pw.println(s"${i0}assign ${srcSig.name} = ${dstSig.name};")
        case _                            =>
      }
      (dstPort.accept, srcPort.accept) match {
        case (Some(dstSig), Some(srcSig)) => pw.println(s"${i0}assign ${srcSig.name} = ${dstSig.name};")
        case _                            =>
      }
      (dstPort.accept, srcPort.ready) match {
        case (Some(dstSig), Some(srcSig)) => pw.println(s"${i0}assign ${srcSig.name} = ${dstSig.name};")
        case _                            =>
      }

      pw.println()
    }

  }

  def finishState(task: StateTask, pw: PrintWriter): Unit = {
    numstates = task match {
      case StateTask(_, _, _, s, _, _) => s.length
    }
    log2numstates = ceillog2(numstates)

    // Emit header and combinatorial code
    val states = mutable.Map[Int, StrTree]() // Collection of state code
    val clears = Stack[StrTree]() // Collection of outputs to clear if !go
    val defaults = Stack[StrTree]() // Collection of things to set at start of each cycle
    val acceptdefaults = Stack[StrTree]() // Collection of things to set at start of accept generation
    val clocks = Stack[StrTree]() // Collection of things to clock if go
    val clocks_no_reset = Stack[StrTree]() // Collection of things to clock if go but do not need reset
    val resets = Stack[StrTree]() // Collection of things to reset
    val clock_clears = Stack[StrTree]() // Collection of how to clear sync outputs
    val acceptstates = mutable.Map[Int, StrTree]() // Collection of code to generate accept outputs for each state

    def writeOutNxt(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType => {
        val wstr = if (kind.widthExpr.isKnown) s"${kind.widthExpr.eval}" else ""
        pw.println(s"  reg " + Signal(nx(name), kind).declString + ";")
        resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= ${wstr}'b0;\n") :: Nil)
        clocks push StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
        defaults push StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil)
      }
      case (_: Struct | VoidType) => /* Nothing to do */
    }
    def writeVarInternal(typ: Type, name: StrTree, resetToZero: Boolean): Unit = {
      val nm = name.toString
      typ match {
        case kind: ScalarType => {
          pw.println(s"  reg " + Signal(reg(nm), kind).declString + ";")
          pw.println(s"  reg " + Signal(nx(nm), kind).declString + ";")
          if (resetToZero) {
            val wstr = if (kind.widthExpr.isKnown) s"${kind.widthExpr.eval}" else ""
            resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= ${wstr}'b0;\n") :: Nil)
          }
          clocks push StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
          defaults push StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil)
        }
        case _ =>
      }
    }
    def writeVarWithReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, true)
    def writeVarWithNoReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, false)

    var generateAccept = false // As an optimization, don't bother generating accept for modules without any ports that require it

    // Emit port declarations
    id2decl.values foreach {
      case DeclOut(kind, name, fctype, stype) => {
        if (fctype.hasValid) {
          if (stype == StorageTypeWire) {
            clears push Str("      " + nx(valid(name)) + " = 1'b0;\n")
          }
          if (stype == StorageTypeReg && fctype == FlowControlTypeReady) {
            defaults push Str("    " + nx(valid(name)) + " = " + valid(name) + " && !" + ready(name) + ";\n")
          } else {
            defaults push Str("    " + nx(valid(name)) + " = 1'b0;\n")
          }
        }
        if (stype == StorageTypeWire) {
          defaults push Str("    " + nx(name) + " = 'b0;\n")
        }
        (fctype, stype) match {
          case (FlowControlTypeValid, StorageTypeReg) => clock_clears push Str("      " + valid(name) + " <= 1'b0;\n")
          case _                                      =>
        }
        if (fctype.hasReady) {
          clock_clears push StrList(
            Str("      if (" + ready(name) + ") begin\n") ::
              Str("        " + valid(name) + " <= 1'b0;\n") ::
              Str("      end\n") :: Nil)
          //StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
        }

        (fctype, stype) match {
          case (FlowControlTypeValid, StorageTypeReg) | (FlowControlTypeReady, StorageTypeReg) => {
            val v = valid(name)
            resets push StrList(Str("      ") :: Str(v) :: Str(" <= 1'b0;\n") :: Nil)
            clocks push StrList(Str("        ") :: Str(v) :: Str(" <= ") :: Str(nx(v)) :: Str(";\n") :: Nil)
          }
          case (FlowControlTypeReady, StorageTypeBubble) => {
            val v = valid(name)
            resets push StrList(Str("      ") :: Str(v) :: Str(" <= 1'b0;\n") :: Nil)
            clocks push StrList(Str("        ") :: Str(v) :: Str(" <= ") :: Str(nx(v)) ::
              Str(" || (") :: Str(v) :: Str(" && !") :: Str(ready(name)) :: Str(");\n") :: Nil)
          }
          case _ =>
        }

      }
      case DeclIn(kind, name, fctype) => {
        if (fctype.hasReady) {
          clears push Str("      " + ready(name) + " = 1'b0;\n")
          defaults push Str("    " + ready(name) + " = 1'b0;\n")
        }
        if (fctype.hasAccept) {
          generateAccept = true;
          acceptdefaults push Str("    " + accept(name) + " = 1'b0;\n")
        }
      }
      case _ =>
    }
    // Emit remaining variable declarations
    id2decl.values foreach {
      case DeclOut(kind, name, fctype, stype) => {
        if (stype != StorageTypeWire) {
          VisitType(kind, name)(writeOutNxt) // declare nxt values for outputs
          if (fctype.hasValid) {
            pw.println("  reg " + nx(valid(name)) + ";")
          }
        }
      }
      case DeclArr(kind, name, index :: Nil) => {
        // TODO maybe figure out the number of bits in the type and declare as this many?
        // TODO maybe detect more than one write to the same array in the same cycle?
        Arrays.add(name)
        val depth = index.eval.toInt // TODO detect if this fails and fail gracefully
        val log2depth = ceillog2(depth) max 1 // TODO: handle degenerate case of depth == 1 better
        //        val t = typeString(kind)
        pw.println(s"  reg ${name}_wr;")
        pw.println(s"  reg ${Signal(name + "_wrdata", kind).declString};")
        pw.println(s"  reg [${log2depth - 1}:0] ${name}_wraddr;")
        pw.println(s"  reg ${Signal(name, kind).declString} [${depth - 1}:0];")
        defaults push StrList(
          Str(s"    ${name}_wr = 1'b0;\n") ::
            Str(s"    ${name}_wraddr = 'b0;\n") ::
            Str(s"    ${name}_wrdata = 'b0;\n") :: Nil)
        clears push Str(s"      ${name}_wr = 1'b0;\n")
        clocks_no_reset push Str(s"""|${i0 * 3}if (${name}_wr) begin
                                         |${i0 * 4}${name}[${name}_wraddr] <= ${name}_wrdata;
                                         |${i0 * 3}end
                                         |""".stripMargin)
      }
      case DeclVar(kind, name, None) => {
        VisitType(kind, name)(writeVarWithReset)
      }
      case DeclVar(kind, name, Some(init)) => {
        VisitType(kind, name)(writeVarWithNoReset)
        resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= ") :: MakeExpr(init) :: Str(";\n") :: Nil)
      }
      case _ =>
    }

    // Declare 'go'
    pw.println("  reg go;")

    // Construct always block contents
    task.states foreach {
      case blk @ StateBlock(_, n, _) => {
        val indent = if (numstates == 1) 2 else 3
        states(n) = MakeStmt(indent)(blk)
        if (generateAccept) {
          makingAccept = true
          AcceptStmt(indent, blk) match {
            case Some(x) => acceptstates(n) = x
            case None    =>
          }
          makingAccept = false
        }
      }
    }

    val fencefn = task.fencefn map { f => MakeStmt(2)(f.body) }

    // Emit all verilog functions
    task.vfns foreach { vfn =>
      pw.print(vfn.body)
    }

    // Automatic stall generation
    val disable_name = "DISABLE_STALL_" + task.name.toUpperCase() + "_V"
    pw.println(s"""`ifdef ENABLE_STALL
`ifdef ${disable_name}
`else
reg [31:0] go_stall_prob;
reg [31:0] go_rand;
always @(posedge clk or negedge rst_n) begin
    if (!rst_n) begin
        go_stall_prob <= ($$unsigned($$random()) % 32'd90) + 5;
        go_rand <= 32'd0;
    end else begin
        go_rand <= $$unsigned($$random()) % 32'd100;
    end
end
`endif
`endif""")

    // Start main combinatorial loop
    pw.println()
    pw.println("  always @* begin")
    pw.println("    go = 1'b1;")

    // Insert automatic stall generation
    pw.println(s"""`ifdef ENABLE_STALL
`ifdef ${disable_name}
`else
    go = go && (go_rand < go_stall_prob);
`endif
`endif""")

    // Prepare defaults
    if (defaults.length > 0)
      pw.println(StrList(defaults))
    fencefn foreach pw.println

    if (numstates == 1) {
      pw.println(s"    ${states(0)}")
    } else if (numstates > 1) {
      pw.println("    case (state)")
      pw.println("      default: begin")
      pw.println("      end")
      for ((n, c) <- states.toList.sortBy(_._1)) {
        pw.println(s"      ${MakeState(n)}: ${c}")
      }
      pw.println("    endcase")
    }

    pw.println()
    if (clears.length > 0) {
      pw.println(s"    if (!$go) begin")
      pw.print(StrList(clears))
      pw.println("    end")
    }
    pw.println("  end")

    // Generate accept output if used
    if (generateAccept) {
      pw.println()
      pw.println("  always @* begin")
      if (acceptdefaults.length > 0)
        pw.println(StrList(acceptdefaults))
      if (numstates == 1) {
        pw.println(s"    ${acceptstates(0)}")
      } else if (numstates > 1) {
        pw.println("    case (state)")
        pw.println("      default: begin")
        pw.println("      end")
        for ((n, c) <- acceptstates.toList.sortBy(_._1)) {
          pw.println(s"      ${MakeState(n)}: ${c}")
        }
        pw.println("    endcase")
      }
      // If using automatic stalls may need to cancel accept
      pw.println(s"""`ifdef ENABLE_STALL
`ifdef ${disable_name}
`else
    if ( !(go_rand < go_stall_prob) ) begin""")
      pw.println(StrList(acceptdefaults))
      pw.println("""    end
`endif
`endif""")
      pw.println("  end")
      pw.println()
    }

    // Now emit clocked blocks
    pw.println()
    pw.println("  always @(posedge clk or negedge rst_n) begin")
    pw.println("    if (!rst_n) begin")
    pw.print(StrList(resets))
    pw.println("    end else begin")
    pw.print(StrList(clock_clears))
    pw.println(s"      if ($go) begin")
    pw.print(StrList(clocks))
    pw.println("      end")
    pw.println("    end")
    pw.println("  end")

    if (!clocks_no_reset.isEmpty) {
      pw.println()
      pw.println(s"  always @(posedge clk) begin")
      pw.println(s"    if ($go) begin")
      pw.print(StrList(clocks_no_reset.toList))
      pw.println(s"    end")
      pw.println(s"  end")
    }
    pw.println()
  }

  // We would prefer to use _d and _q, except that it is more useful
  // for mixing with verilog and for output ports to use the name without a suffix for direct access

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(name: String): String = if (makingAccept) {
    // IdsUsedToMakeAccept.add(name)
    if (IdsWritten contains name) {
      cc.fatal(s"Cannot have the use of an accept port conditional on $name because it is written in the same cycle.")
    }
    name
  } else nxMap(name)

  def nx(names: List[String]): String = nx(names.mkString("_"))
  def nx(name: StrTree): String = nx(name.toString)

  // Construct the string to be used when this identifier is used on the LHS of an assignment
  def reg(names: List[String]): String = regMap(names.mkString("_"))
  def reg(name: StrTree): String = regMap(name.toString)

  implicit def string2StrTree(s: String): StrTree = Str(s)

  // Construct a string for an expression
  def MakeExpr(tree: Expr): StrTree = {
    tree match {
      case ExprArrIndex(_, name, index) => {
        val indices = index map MakeExpr mkString ("[", "][", "]")
        StrList(List(MakeExpr(name), Str(indices)))
      }
      case ExprVecIndex(_, ref, index) => {
        val indices = index map MakeExpr mkString ("[", "][", "]")
        StrList(List(MakeExpr(ref), Str(indices)))
      }
      case Slice(_, ref, l, op, r) => StrList(List(MakeExpr(ref), "[", MakeExpr(l), op, MakeExpr(r), "]"))
      case ValidCall(_, DottedName(_, names)) => id2decl(names.head) match {
        case DeclOut(decl, n, fctype, _) => if (fctype.hasValid) valid(n) else { cc.fatal(tree, s"Port '$names' does not use valid"); "" }
        case DeclIn(decl, n, fctype)     => if (fctype.hasValid) valid(n) else { cc.fatal(tree, s"Port '$names' does not use valid"); "" }
        case _                           => cc.fatal(tree, s"Cannot access valid on $names"); ""
      }
      case Zxt(_, numbitsExpr, expr) => expr.widthExpr match {
        case None => cc.fatal(expr, s"Cannot compute size of expression '${expr.toSource}' for '@zx'")
        case Some(widthExpr) => {
          val deltaExpr = (numbitsExpr - widthExpr).simplify
          StrList(List("{{", MakeExpr(deltaExpr), "{1'b0}},", MakeExpr(expr), "}"))
        }
      }
      case Sxt(_, numbitsExpr, expr) => expr.widthExpr match {
        case None => cc.fatal(expr, s"Cannot compute size of expression '${expr.toSource}' for '@sx'")
        case Some(widthExpr) => {
          val deltaExpr = (numbitsExpr - widthExpr).simplify
          val msbExpr = expr.msbExpr match {
            case None       => cc.fatal(expr, s"Cannot compute msb of expression '${expr.toSource}' for '@sx'")
            case Some(expr) => expr.simplify
          }
          StrList(List("{{", MakeExpr(deltaExpr), "{", MakeExpr(msbExpr), "}},", MakeExpr(expr), "}"))
        }
      }
      case DollarCall(_, name, args) => StrList(List(name, "(", StrList(args.map(MakeExpr), ","), ")"))
      case ReadCall(_, name) => id2decl(name.names.head) match {
        case DeclIn(VoidType, _, _) => {
          cc.error(tree, s"Cannot read 'void' port '${name.toSource}' in expression position"); Str("")
        }
        case _ => MakeExpr(name)
      }

      case BinaryOp(_, lhs, op, rhs)    => StrList(List(MakeExpr(lhs), Str(" "), op, Str(" "), MakeExpr(rhs)))
      case UnaryOp(_, op, lhs)          => StrList(List(op, MakeExpr(lhs)))
      case Bracket(_, content)          => StrList(List("(", MakeExpr(content), ")"))
      case TernaryOp(_, cond, lhs, rhs) => StrList(List(MakeExpr(cond), " ? ", MakeExpr(lhs), " : ", MakeExpr(rhs)))
      case BitRep(_, count, value)      => StrList(List("{", MakeExpr(count), "{", MakeExpr(value), "}}"))
      case BitCat(_, parts)             => StrList(List("{", StrList(parts.map(MakeExpr), ","), "}"))
      case DottedName(_, names)         => nx(names)
      case Literal(_, s)                => Str(s)
      case n: Num                       => n.toVerilog
      case e: ErrorExpr                 => e.toVerilog
      case e                            => cc.ice(tree, s"Unexpected expression '$e'"); ""
    }
  }

  // Called with an integer representing a state, returns the appropriate string
  def MakeState(state: Int): String = s"$log2numstates'd$state"

  // This function defines how to write next values (D input on flip-flops)
  //def nx(x: String): StrTree = StrList(List(x, Str("_nxt")))

  // Produce code to go into the case statements (we assume we have already had a "case(state) default: begin end"
  // The top call should be with Function or VerilogFunction
  def MakeStmt(indent: Int)(tree: Node): StrTree = {
    val i = i0 * indent

    // Take a combinatorial statement and return stall conditions that should be emitted now based on ids that are read/written
    // This code will be emitted before the actual statement
    // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
    def StallExpr(tree: Node): List[String] = {
      // Use a local function to avoid having to copy emitted list down the stack
      var blockingStatements: List[String] = Nil

      def add(s: String): Unit = blockingStatements = s :: blockingStatements

      def AddRead(name: DottedName, isLock: Boolean): Boolean = {
        val n: String = ExtractName(name)
        val d: Decl = id2decl(n)
        d match {
          case DeclIn(_, _, fctype) => {
            if (fctype.hasValid)
              add(s"$go = $go && ${valid(n)};")
            if (!isLock && fctype.hasReady)
              add(ready(n) + " = 1'b1;")
          }
          case _ => cc.fatal(name, s"'$name' cannot be read"); false // TODO check this earlier?
        }
        false // No need to recurse
      }

      def v(tree: Node): Boolean = tree match {
        case CombinatorialCaseStmt(_, value, _, _) =>
          value visit v; false
        case CombinatorialBlock(_, _) => false
        case CombinatorialIf(_, cond, _, _) =>
          cond visit v; false
        case ReadCall(_, name) => AddRead(name, false)
        case WaitCall(_, name) => AddRead(name, true)
        case WriteCall(_, name, _) => {
          val n: String = ExtractName(name)
          val d: Decl = id2decl(n)
          d match {
            case DeclOut(_, _, fctype, stype) => {
              (fctype, stype) match {
                case (FlowControlTypeReady, StorageTypeBubble) => add(s"$go = $go && !${valid(n)};")
                case (FlowControlTypeReady, StorageTypeReg)    => add(s"$go = $go && (!${valid(n)} || ${ready(n)});")
                case (FlowControlTypeAccept, StorageTypeReg)   => cc.fatal(tree, "sync accept only supported as wire output type") // TODO check this earlier
                case (FlowControlTypeAccept, StorageTypeWire)  => add(s"$go = $go && ${accept(n)};")
                case _                                         =>
              }
              if (fctype.hasValid)
                add(s"${nx(valid(n))} = 1'b1;")
            }
            case _ => cc.fatal(name, s"'$name' cannot be written"); false // TODO check this earlier?
          }
          true // Recurse in case arguments use reads
        }
        case _ => true
      }
      tree visit v
      return blockingStatements
    }

    def AddStall(terms: Node*)(expr: StrTree): StrTree = {
      val stalls = terms.toList flatMap StallExpr
      if (stalls.isEmpty) {
        expr
      } else {
        val s = stalls map { x => Str(x) }
        // TODO remove string interpolation/mkString and use StrTree for speed
        Str(s"""|begin
                |${i + i0}${s mkString s"\n${i + i0}"}
                |${i + i0}${expr}
                |${i}end""".stripMargin)
      }
    }

    // Slice expression for call_depth to special case various values of CALL_STACK_SIZE
    lazy val call_depth_slice = {
      val css = id2decl("CALL_STACK_SIZE") match {
        case DeclConst(_, _, init: Expr) => init.eval.toInt
        case _                           => unreachable
      }
      assert(css > 1)
      if (css == 2) {
        // CALL_STACK_SIZE == 2 means there is 1 stack storage location,
        // in which case call_depth is declared as a plain reg,
        // so no need to index it
        ""
      } else {
        // Otherwise take the right number of bits. this is necesary
        // as the call stack array has one less element than the maximum
        // value of call_depth, so call_dapth might have 1 more bit
        // than the array address requires (if CALL_STACK_SIZE = 2**N + 1
        s"[${ceillog2(css - 1) - 1}:0]"
      }
    }

    tree match {
      case Assign(_, LValArrayLookup(_, LValName(_, n :: _), index :: Nil), rhs) if (Arrays contains n) => AddStall(index, rhs) {
        Str(s"""|begin
                |${i + i0}${n}_wr = 1'b1;
                |${i + i0}${n}_wraddr = ${MakeExpr(index)};
                |${i + i0}${n}_wrdata = ${MakeExpr(rhs)};
                |${i}end""".stripMargin)
      }
      case Assign(_, lhs, rhs) => AddStall(lhs, rhs) {
        Str(s"${MakeExpr(lhs.toExpr)} = ${MakeExpr(rhs)};")
      }

      case CombinatorialBlock(_, cmds) => {
        // TODO remove string interpolation/mkString and use StrTree for speed
        Str(s"""|begin
                |${i + i0}${cmds map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}end""".stripMargin)
      }

      case StateBlock(a, _, cmds) => MakeStmt(indent)(CombinatorialBlock(a, cmds))

      case CombinatorialIf(_, cond, thenBody, optElseBody) => AddStall(cond) {
        // TODO remove string interpolation/mkString and use StrTree for speed
        val condPart = s"if (${MakeExpr(cond)}) "
        val thenPart = thenBody match {
          case block: CombinatorialBlock => MakeStmt(indent)(block)
          case other                     => MakeStmt(indent)(CombinatorialBlock(other.attr, other :: Nil))
        }
        val elsePart = optElseBody match {
          case None                            => ""
          case Some(block: CombinatorialBlock) => s" else ${MakeStmt(indent)(block)}"
          case Some(other)                     => s" else ${MakeStmt(indent)(CombinatorialBlock(other.attr, other :: Nil))}"
        }
        Str(condPart + thenPart + elsePart)
      }

      // TODO: do the conds not need a stall expr to be precise?
      case CombinatorialCaseStmt(_, value, cases, None) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseStmt(_, value, cases, Some(default)) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i + i0}default: ${MakeStmt(indent + 1)(default)}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseLabel(_, conds, body) => Str(s"${conds map MakeExpr mkString ", "}: ${MakeStmt(indent)(body)}")

      case GotoState(_, target) => {
        if (numstates == 1) {
          Str("")
        } else {
          StrList(List(nx("state"), " = ", MakeState(target), ";"))
        }
      }

      case CallState(_, tgt, ret) => {
        Str(s"""|begin
                |${i + i0}call_stack_wr = 1'b1;
                |${i + i0}call_stack_wraddr = call_depth${call_depth_slice};
                |${i + i0}call_stack_wrdata = ${MakeState(ret)};
                |${i + i0}call_depth_nxt = call_depth + 1'b1;
                |${i + i0}${nx("state")} = ${MakeState(tgt)};
                |${i}end""".stripMargin)
        // TODO: Add assertion for call_depth_nxt < CALL_STACK_SIZE
      }

      case _: ReturnState => {
        Str(s"""|begin
                |${i + i0}call_depth_nxt = call_depth - 1'b1;
                |${i + i0}${nx("state")} = call_stack[call_depth_nxt${call_depth_slice}];
                |${i}end""".stripMargin)
      }

      case ExprStmt(_, WaitCall(_, _))       => AddStall(tree) { Str("") }
      case ExprStmt(_, ReadCall(_, _))       => AddStall(tree) { Str("") }
      case ExprStmt(_, WriteCall(_, _, Nil)) => AddStall(tree) { Str("") }
      case ExprStmt(_, WriteCall(_, name, arg :: Nil)) => AddStall(tree) {
        Str(s"${MakeExpr(name)} = ${MakeExpr(arg)};")
      }

      case DeclarationStmt(a, DeclVar(kind, id, Some(rhs))) => MakeStmt(indent)(Assign(a, LValName(a, id :: Nil), rhs))
      case DeclarationStmt(a, DeclVar(kind, id, None)) => MakeStmt(indent)(Assign(a, LValName(a, id :: Nil), Num(a, false, None, 0))) // TODO: Why is this needed ?

      case ExprStmt(_, DollarCall(_, name, args)) => StrList(List(name, "(", StrList(args.map(MakeExpr), ","), ");"))
      case AlogicComment(_, s) => s"// $s\n"

      case ErrorStmt(_) | ExprStmt(_, _: ErrorExpr) => Str("/*Error statment*/")

      case x => cc.ice(x, s"Don't know how to emit code for $x"); Str("")
    }
  }

  // Note that valid can depend on accept
  //      and ready can depend on valid
  //
  // Accept is useful when we want low-latency wire communications from A->B (e.g. A is decoding bits in a tight feedback loop with B)
  // We cannot use sync ready because A's valid depends on B's ready (as this is a wire port), and B's ready depends on A's valid.
  // Instead we use sync accept.
  // B outputs accept if it contains a p.read() in the current state
  // The accept should only be based on B's local registers, and constructed in a separate always_comb block.
  //
  // If we assert accept we are contractually obliged to go if and only if the valid ends up high.
  //
  // Therefore we need to check:
  //   There are no other stalling port reads/writes in this state
  //   None of the variables that affect whether p.read() is called are written in this state, we call forbid to ban assigns to these registers
  //
  // As an optimization we can skip all of this if there are no sync accept ports
  //
  // While we are generating accept, the nx function is adjusted to point to the reg version instead.
  // It also captures the ids that are used so we can check they are stable

  val IdsUsedToMakeAccept = mutable.Set[String]() // This allows us to flag errors where the accept signal would be generated incorrectly
  var IdsWritten = mutable.Set[String]()

  val syncPortsFound = mutable.Set[String]() // This allows us to flag errors if we use two accept ports in the same state

  var usesPort: Option[String] = None // This allows us to flag errors if another port is read

  // Combine a list of stalls (often empty) with an optional body (often empty)
  // Return None if result is empty
  def AddAccept(indent: Int, stalls: List[String], expr: Option[StrTree]): Option[StrTree] = {
    if (stalls.isEmpty)
      expr
    else {
      val i = i0 * indent
      val e: List[StrTree] = expr match {
        case Some(x) => x :: Nil
        case None    => Nil
      }
      Some(StrList(Str(i) :: Str("begin\n") ::
        stalls.map(x => Str(i + x)) :::
        e :::
        Str(i) :: Str("end\n") :: Nil))
    }
  }

  def AcceptStmt(indent: Int, tree: Node): Option[StrTree] = tree match {
    case Assign(_, lhs, rhs) => {
      IdsWritten.add(ExtractName(lhs))
      AddAccept(indent, AcceptExpr(lhs) ::: AcceptExpr(rhs), None)
    }
    case CombinatorialCaseStmt(_, value, cases, Some(default)) => {
      // Take care to only use MakeExpr when we are sure the code needs to be emitted
      // This is because MakeExpr will track the used ids

      // TODO add support for resetting IdsWritten between each statement, and form union of used ids at end
      // At the moment the warning about symbol use will be over-protective here.

      val s: List[Option[StrTree]] = for (c <- cases) yield AcceptStmt(indent + 1, c)
      val s2: List[StrTree] = (AcceptStmt(indent + 1, default) :: s).flatten
      val e = if (s2.length == 0)
        None
      else
        Some(StrList(Str(i0 * indent + "case(") :: MakeExpr(value) :: Str(") begin\n") ::
          StrList(s2) :: Str(i0 * indent) :: Str("endcase\n") :: Nil))
      AddAccept(indent, AcceptExpr(value), e)
    }
    case CombinatorialIf(_, cond, body, Some(elsebody)) =>
      {
        val initialIds = IdsWritten.clone()
        val b = AcceptStmt(indent + 1, body)
        val IdsWithBody = IdsWritten
        IdsWritten = initialIds
        val eb = AcceptStmt(indent + 1, elsebody)
        val IdsWithElse = IdsWritten
        val gen = b.isDefined || eb.isDefined
        val bs = b match {
          case Some(a) => a
          case None    => Str(i0 * indent + "begin\n" + i0 * indent + "end\n")
        }
        val ebs = eb match {
          case Some(a) => a
          case None    => Str(i0 * indent + "begin\n" + i0 * indent + "end\n")
        }
        IdsWritten = initialIds
        val e = if (gen)
          Some(StrList(Str(i0 * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: bs :: Str(i0 * indent) :: Str("else\n") :: ebs :: Nil))
        else
          None
        IdsWritten = IdsWithBody union IdsWithElse
        AddAccept(indent, AcceptExpr(cond), e)
      }
    case CombinatorialIf(_, cond, body, None) => {
      val initialIds = IdsWritten.clone()
      val b = AcceptStmt(indent + 1, body)
      val IdsWithBody = IdsWritten
      IdsWritten = initialIds
      // We take care to only call MakeExpr when the body would have something to generate
      // This avoids forbidding ids that are not actually important for generating accept
      val e = b match {
        case None    => None
        case Some(a) => Some(StrList(Str(i0 * indent) :: Str("if (") :: MakeExpr(cond) :: Str(")\n") :: a :: Nil))
      }
      IdsWritten = IdsWithBody
      AddAccept(indent, AcceptExpr(cond), e)
    }

    case WaitCall(_, name)                              => AddAccept(indent, AcceptExpr(name), None)
    case WriteCall(_, name, args) if (args.length == 1) => AddAccept(indent, AcceptExpr(name) ::: AcceptExpr(args(0)), None)
    case CombinatorialBlock(_, cmds) => {
      val s: List[Option[StrTree]] = for (c <- cmds) yield AcceptStmt(indent + 1, c)
      val s2: List[StrTree] = s.flatten
      if (s2.length == 0)
        None
      else
        Some(StrList(Str(i0 * indent) :: Str("begin\n") :: StrList(s2) :: Str(i0 * indent) :: Str("end\n") :: Nil))
    }

    case DeclarationStmt(a, DeclVar(_, id, Some(rhs))) => AcceptStmt(indent, Assign(a, LValName(a, id :: Nil), rhs))
    case StateBlock(a, state, cmds) => {
      // Clear sets used for tracking
      syncPortsFound.clear()
      //IdsUsedToMakeAccept.clear()
      IdsWritten.clear()
      usesPort = None
      // See if there is anything to do for this state
      val s = for { cmd <- cmds } yield AcceptStmt(indent, cmd)
      val s2 = s.flatten
      if (s2.length > 0) {
        // Check for error conditions
        // TODO change sync ports into a set so only warn if different ports detected
        if (syncPortsFound.size > 1) cc.fatal(tree, s"Cannot access multiple accept port reads in same cycle: $cmds")
        if (usesPort.isDefined) cc.fatal(tree, s"Cannot access port $usesPort while generating accept: $cmds")
        //if (!IdsUsedToMakeAccept.intersect(IdsWritten).isEmpty) cc.warning(s"Accept is based on registered signals: check condition does not depend on a written identifier: $cmds")
        //It seems that now the state is emitted by higher level code?
        //Some(StrList(List(i0 * (indent - 1), MakeState(state), ": begin\n", StrList(s2), i0 * (indent - 1), "end\n")))
        Some(StrList(List("begin\n", StrList(s2), i0 * (indent), "end\n")))
      } else
        None
    }
    case CombinatorialCaseLabel(_, Nil, body) => {
      val b = AcceptStmt(indent + 1, body)
      b match {
        case None    => None
        case Some(a) => Some(StrList(Str(i0 * indent) :: Str("default:\n") :: a :: Nil))
      }
    }
    case CombinatorialCaseLabel(_, conds, body) => {
      val b = AcceptStmt(indent + 1, body)
      val e = b match {
        case None => None
        case Some(a) => Some(StrList(
          Str(i0 * indent) ::
            StrList(conds.map(MakeExpr), ",") ::
            Str(":\n") :: a :: Nil))
      }
      val as = conds.map(AcceptExpr).flatten
      AddAccept(indent, as, e)
    }

    case x => None
  }

  // Take a combinatorial statement and return accept statements that should be emitted now based on ids that are read/written
  // This code will be emitted before the actual statement
  // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
  // Strings have \n at the end, but indent will be added later
  def AcceptExpr(tree: Node): List[String] = {
    // Use a local function to avoid having to copy emitted list down the stack
    var blockingStatements: List[String] = Nil

    def add(s: String): Unit = blockingStatements = s :: blockingStatements

    def AddRead(name: DottedName): Boolean = {
      val n: String = ExtractName(name)
      id2decl(n) match {
        case DeclIn(_, _, fctype) => {
          if (fctype.hasAccept) {
            syncPortsFound.add(n)
            add(accept(n) + " = 1'b1;\n")
          }
          if (fctype.hasReady)
            usesPort = Some(MakeExpr(name).toString)
        }
        case _ =>
      }
      false // No need to recurse
    }

    def v(tree: Node): Boolean = tree match {
      case CombinatorialCaseStmt(_, value, _, _) =>
        value visit v; false
      case CombinatorialBlock(_, _) => false
      case CombinatorialIf(_, cond, _, _) =>
        cond visit v; false
      case ReadCall(_, name) => AddRead(name)
      case WaitCall(_, name) => AddRead(name)
      case WriteCall(_, name, _) => {
        val n: String = ExtractName(name)
        val d: Decl = id2decl(n)
        d match {
          case DeclOut(_, _, fctype, _) => {
            if (fctype.hasValid)
              usesPort = Some(MakeExpr(name).toString)
          }
          case _ => false
        }
        true // Recurse in case arguments use reads
      }
      case _ => true
    }
    tree visit v
    return blockingStatements
  }

}
