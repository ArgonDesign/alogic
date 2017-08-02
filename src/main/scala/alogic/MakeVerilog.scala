////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
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

final class MakeVerilog(moduleCatalogue: Map[String, Task]) {

  val i0 = "  "; // Single indentation depth (2 spaces)

  val id2decl = mutable.Map[String, Declaration]()

  val nxMap = mutable.Map[String, String]() // Returns string to use when this identifier is accessed
  val regMap = mutable.Map[String, String]() // Map from name in alogic to name in Verilog

  // Map of names used to instantiate modules to multiplicity of that name
  val namecnt = mutable.Map[String, Int]() withDefaultValue (0)

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

  def apply(task: Task, opath: Path): Unit = {
    val Task(modname, decls) = task // Save the name of this module

    val outtype = task match {
      case _: StateTask   => "reg "
      case _: FsmTask     => "reg "
      case _: NetworkTask => "wire "
      case _: VerilogTask => "wire "
    }

    // This controls whether we will declare _nxt signals for relevant output ports
    val declareExtraOut = task match {
      case _: StateTask   => true
      case _: FsmTask     => true
      case _: NetworkTask => false
      case _: VerilogTask => false
    }

    numstates = task match {
      case StateTask(_, _, s, _, _) => s.length
      case _                        => 0
    }
    log2numstates = ceillog2(numstates)

    // Collect all the declarations
    decls foreach { x => id2decl(x.id) = x }
    task visit {
      // We remove the initializer from declaration statements
      // These will be reset inline where they are declared.
      case DeclarationStmt(VarDeclaration(decltype, id, _)) => { id2decl(id) = VarDeclaration(decltype, id, None); false }
      case _ => true
    }

    // Emit header and combinatorial code
    val states = mutable.Map[Int, StrTree]() // Collection of state code
    val fencefns = Stack[StrTree]() // Collection of fence function code
    val clears = Stack[StrTree]() // Collection of outputs to clear if !go
    val defaults = Stack[StrTree]() // Collection of things to set at start of each cycle
    val clocks = Stack[StrTree]() // Collection of things to clock if go
    val clocks_no_reset = Stack[StrTree]() // Collection of things to clock if go but do not need reset
    val resets = Stack[StrTree]() // Collection of things to reset
    val verilogfns = Stack[StrTree]() // Collection of raw verilog text
    val clock_clears = Stack[StrTree]() // Collection of how to clear sync outputs
    val acceptstates = mutable.Map[Int, StrTree]() // Collection of code to generate accept outputs for each state

    opath.createFile(createParents = true, failIfExists = false)

    val pw = new PrintWriter(new File(opath.path))

    def writeOut(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType => {
        pw.println("  output " + outtype + Signal(name.toString, kind).declString + ",")
      }
      case _: Struct => /* Nothing to do */
    }
    def writeOutNxt(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType => {
        pw.println(s"  reg " + Signal(nx(name), kind).declString + ";")
        resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= 'b0;\n") :: Nil)
        clocks push StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
        defaults push StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil)
      }
      case _: Struct => /* Nothing to do */
    }
    def writeIn(typ: Type, name: StrTree): Unit = typ match {
      case kind: ScalarType => pw.println("  input wire " + Signal(name.toString, kind).declString + ",")
      case _: Struct        => /* Nothing to do */
    }
    def writeVarInternal(typ: Type, name: StrTree, resetToZero: Boolean): Unit = {
      val nm = name.toString
      typ match {
        case kind: ScalarType => {
          pw.println(s"  reg " + Signal(reg(nm), kind).declString + ";")
          pw.println(s"  reg " + Signal(nx(nm), kind).declString + ";")
          if (resetToZero) {
            resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= 'b0;\n") :: Nil)
          }
          clocks push StrList(Str("        ") :: Str(reg(name)) :: Str(" <= ") :: Str(nx(name)) :: Str(";\n") :: Nil)
          defaults push StrList(Str("    ") :: Str(nx(name)) :: Str(" = ") :: Str(reg(name)) :: Str(";\n") :: Nil)
        }
        case _ =>
      }
    }
    def writeVarWithReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, true)
    def writeVarWithNoReset(typ: Type, name: StrTree): Unit = writeVarInternal(typ, name, false)

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
      case InDeclaration(_, decltype, name) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case OutDeclaration(fctype, decltype, name, stype) => {
        stype match {
          case StorageTypeWire => SetNxType(nxMap, decltype, name, "")
          case _               => SetNxType(nxMap, decltype, name, "_nxt")
        }
        SetNxType(regMap, decltype, name, "")
        if (HasValid(fctype)) {
          SetNxType(regMap, IntType(false, 1), valid(name), "")
          if (stype == StorageTypeWire)
            SetNxType(nxMap, IntType(false, 1), valid(name), "")
          else
            SetNxType(nxMap, IntType(false, 1), valid(name), "_nxt")
        }
      }
      case ParamDeclaration(decltype, name, init) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case ConstDeclaration(decltype, name, init) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case VarDeclaration(decltype, name, _) => {
        SetNxType(nxMap, decltype, name, "_nxt")
        SetNxType(regMap, decltype, name, "")
      }
      case ArrayDeclaration(decltype, name, _) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case VerilogVarDeclaration(decltype, name) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case VerilogArrayDeclaration(decltype, name, _) => {
        SetNxType(nxMap, decltype, name, "")
        SetNxType(regMap, decltype, name, "")
      }
      case x => Message.ice(s"Don't know how to handle ${x}") // TODO: turn this into ice
    }

    var generateAccept = false // As an optimization, don't bother generating accept for modules without any ports that require it

    pw.println(s"`default_nettype none")
    pw.println()

    pw.print(s"module $modname ")

    val paramDecls = id2decl.values collect {
      case x: ParamDeclaration => x
    }

    if (paramDecls.isEmpty) {
      pw.println(s"(")
    } else {
      pw.println(s"#(")
      // Emit parameter declarations
      val s = for (ParamDeclaration(decltype, id, init) <- paramDecls) yield {
        s"${i0}parameter " + Signal(id, decltype).declString + "=" + MakeExpr(init)
      }
      pw.println(s mkString ",\n")
      pw.println(s") (")
    }

    // Emit port declarations
    id2decl.values foreach {
      case OutDeclaration(fctype, decltype, name, stype) => {
        if (HasValid(fctype)) {
          pw.println("  output " + outtype + valid(name) + ",")
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
        if (HasReady(fctype))
          pw.println("  input wire " + ready(name) + ",")
        if (HasAccept(fctype)) {
          pw.println("  input " + outtype + accept(name) + ",")
        }
        VisitType(decltype, name)(writeOut)
        (fctype, stype) match {
          case (FlowControlTypeValid, StorageTypeReg) => clock_clears push Str("      " + valid(name) + " <= 1'b0;\n")
          case _                                      =>
        }
        if (HasReady(fctype)) {
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
      case InDeclaration(synctype, decltype, name) => {
        if (HasValid(synctype))
          pw.println("  input wire " + valid(name) + ",")
        if (HasReady(synctype)) {
          pw.println("  output " + outtype + ready(name) + ",")
          clears push Str("      " + ready(name) + " = 1'b0;\n")
          defaults push Str("    " + ready(name) + " = 1'b0;\n")
        }
        if (HasAccept(synctype)) {
          generateAccept = true;
          pw.println("  output wire " + accept(name) + ",")
        }
        VisitType(decltype, name)(writeIn)

      }
      case _ =>
    }

    //Goes on bottom so we don't need to special case the lack of comma after the last port
    pw.println("  input wire clk,")
    pw.println("  input wire rst_n")
    pw.println(");\n")

    // Emit localparam (const) declaratoins
    id2decl.values foreach {
      case ConstDeclaration(decltype, id, init) => {
        pw.println(s"${i0}localparam " + Signal(id, decltype).declString + "=" + MakeExpr(init) + ";")
      }
      case _ =>
    }

    // Emit remaining variable declarations
    id2decl.values foreach {
      case OutDeclaration(fctype, decltype, name, stype) => {
        if (declareExtraOut && stype != StorageTypeWire) {
          VisitType(decltype, name)(writeOutNxt) // declare nxt values for outputs
          if (HasValid(fctype)) {
            pw.println("  reg " + nx(valid(name)) + ";")
          }
        }
      }
      case ArrayDeclaration(decltype, name, index :: Nil) => {
        // TODO maybe figure out the number of bits in the type and declare as this many?
        // TODO maybe detect more than one write to the same array in the same cycle?
        Arrays.add(name)
        val depth = MakeExpr(index).toString.toInt // TODO detect if this fails and fail gracefully
        val log2depth = ceillog2(depth) max 1 // TODO: handle degenerate case of depth == 1 better
        //        val t = typeString(decltype)
        pw.println(s"  reg ${name}_wr;")
        pw.println(s"  reg ${Signal(name + "_wrdata", decltype).declString};")
        pw.println(s"  reg [${log2depth - 1}:0] ${name}_wraddr;")
        pw.println(s"  reg ${Signal(name, decltype).declString} [${depth - 1}:0];")
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
      case VarDeclaration(decltype, name, None) => {
        VisitType(decltype, name)(writeVarWithReset)
      }
      case VarDeclaration(decltype, name, Some(init)) => {
        VisitType(decltype, name)(writeVarWithNoReset)
        resets push StrList(Str("      ") :: Str(reg(name)) :: Str(s" <= ") :: MakeExpr(init) :: Str(";\n") :: Nil)
      }
      case _ =>
    }

    task match {
      case network: NetworkTask => {
        assert(network.fsms.isEmpty)
        finishNetwork(network, pw)
      }
      case VerilogTask(_, _, vfns) => {
        // Emit all verilog functions
        vfns foreach { vfn =>
          pw.print(vfn.body)
        }
      }
      case statetask: StateTask => {
        // Declare 'go'
        pw.println("  reg go;")

        // Construct always block contents
        statetask.states foreach {
          case blk @ StateBlock(n, _) => {
            val indent = if (numstates == 1) 2 else 3
            states(n) = MakeStmt(indent)(blk)
            if (generateAccept) {
              makingAccept = true
              AcceptStmt(indent, blk) match {
                case Some(x) => acceptstates(n) = x
                case None =>
              }
              makingAccept = false
            }
          }
        }

        val fencefn = statetask.fencefn map { f => MakeStmt(2)(f.body) }

        // Emit all verilog functions
        statetask.vfns foreach { vfn =>
          pw.print(vfn.body)
        }

        // Start main combinatorial loop
        pw.println()
        pw.println("  always @* begin")
        pw.println("    go = 1'b1;")
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
      case _ => unreachable
    }

    pw.println("endmodule")
    pw.println()
    pw.println(s"`default_nettype wire")
    pw.close()
  }

  def finishNetwork(network: NetworkTask, pw: PrintWriter): Unit = {

    // Collect all instatiations
    val optInstances = for (Instantiate(id, module, args) <- network.instantiate) yield {
      if (!(moduleCatalogue contains module)) {
        Message.error(s"Cannot instantiate undefined module '${module}' in module '${network.name}'")
        None
      } else {
        Some(new ModuleInstance(id, moduleCatalogue(module), args))
      }
    }

    if (optInstances contains None) {
      return
    }

    val instances = optInstances.flatten

    // Construct an instance name -> ModuleInstance map
    val modMap = {
      val pairs = instances map { instance => instance.name -> instance }
      val preMap = immutable.Map("this" -> new ModuleInstance("this", network, Map())) ++ pairs
      preMap withDefault {
        key => Message.fatal(s"Unknown module name '${key}'")
      }
    }

    // Collect port connections
    val portConnections = network.connect flatMap {
      case Connect(DottedName(fromName :: fromPortName :: Nil), to) => {
        val fromInstance = modMap(fromName)
        val fromPort = if (fromName == "this") fromInstance.iwires(fromPortName) else fromInstance.owires(fromPortName)
        to flatMap {
          case DottedName(toName :: toPortName :: Nil) => {
            val toInstance = modMap(toName)
            val toPortOpt = if (toName == "this") toInstance.owires.get(toPortName) else toInstance.iwires.get(toPortName)

            toPortOpt match {
              case None => {
                Message.error(s"No port named '${toPortName}' on instance '${toName}' (module '${toInstance.task.name}')")
                None
              }
              case Some(toPort) => {
                lazy val msg = s"Flow control of port '${fromName}.${fromPortName}' is not compatible with port '${toName}.${toPortName}'"
                (fromPort, toPort) match {
                  case (_: PortNone, _: PortNone)     => // OK
                  case (_: PortNone, _: PortValid)    => Message.error(msg, "none -> sync")
                  case (_: PortNone, _: PortReady)    => Message.error(msg, "none -> sync ready")
                  case (_: PortNone, _: PortAccept)   => Message.error(msg, "none -> sync accept")

                  case (_: PortValid, _: PortNone)    => Message.error(msg, "sync -> none")
                  case (_: PortValid, _: PortValid)   => // OK
                  case (_: PortValid, _: PortReady)   => Message.error(msg, "sync -> sync ready")
                  case (_: PortValid, _: PortAccept)  => Message.error(msg, "sync -> sync accept")

                  case (_: PortReady, _: PortNone)    => Message.error(msg, "sync ready -> none")
                  case (_: PortReady, _: PortValid)   => Message.error(msg, "sync ready -> sync")
                  case (_: PortReady, _: PortReady)   => // OK
                  case (_: PortReady, _: PortAccept)  => // OK (accept can drive ready)

                  case (_: PortAccept, _: PortNone)   => Message.error(msg, "sync accept -> none")
                  case (_: PortAccept, _: PortValid)  => Message.error(msg, "sync accept -> sync")
                  case (_: PortAccept, _: PortReady)  => Message.error(msg, "sync accept -> sync ready")
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
          case DottedName(name :: Nil) if (paramValues contains name) => paramValues(name)
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
        case sig :: Nil => pw.print(s"${i0}assign ${sig} = ")
        case sigs => pw.print(s"""|${i0}assign {
                                  |${i0 * 2}${sigs mkString s",\n${i0 * 2}"}
                                  |${i0}} = """.stripMargin)
      }
      srcSignals match {
        case sig :: Nil => pw.println(s"${sig};")
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

  // We would prefer to use _d and _q, except that it is more useful
  // for mixing with verilog and for output ports to use the name without a suffix for direct access

  // Construct the string to be used when this identifier is used on the RHS of an assignment
  def nx(name: String): String = if (makingAccept) {
   // IdsUsedToMakeAccept.add(name)
    if (IdsWritten contains name) {
      Message.fatal(s"Cannot have the use of an accept port conditional on $name because it is written in the same cycle.")
    }
    name
  } else nxMap(name)

  def nx(names: List[String]): String = nx(names.mkString("_"))
  def nx(name: StrTree): String = nx(name.toString)

  // Construct the string to be used when this identifier is used on the LHS of an assignment
  def reg(names: List[String]): String = regMap(names.mkString("_"))
  def reg(name: StrTree): String = regMap(name.toString)

  implicit def string2StrTree(s: String): StrTree = Str(s)

  implicit def Decl2Typ(decl: Declaration): Type = decl match {
    case ParamDeclaration(decltype, _, _)        => decltype
    case ConstDeclaration(decltype, _, _)        => decltype
    case OutDeclaration(_, decltype, _, _)       => decltype
    case InDeclaration(_, decltype, _)           => decltype
    case VarDeclaration(decltype, _, _)          => decltype
    case ArrayDeclaration(decltype, _, _)        => decltype
    case VerilogVarDeclaration(decltype, _)      => decltype
    case VerilogArrayDeclaration(decltype, _, _) => decltype
    case _: PipelineVarDeclaration               => unreachable
  }

  // Construct a string for an expression
  def MakeExpr(tree: Expr): StrTree = {
    tree match {
      case ArrayLookup(name, index) => {
        val indices = index map MakeExpr mkString ("[", "][", "]")
        StrList(List(MakeExpr(name), Str(indices)))
      }
      case Slice(ref, l, op, r) => StrList(List(MakeExpr(ref), "[", MakeExpr(l), op, MakeExpr(r), "]"))
      case ValidCall(DottedName(names)) => id2decl(names.head) match {
        case OutDeclaration(fctype, decl, n, _) => if (HasValid(fctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case InDeclaration(fctype, decl, n)     => if (HasValid(fctype)) valid(n) else { Message.fatal(s"Port $names does not use valid"); "" }
        case _                                  => Message.fatal(s"Cannot access valid on $names"); ""
      }
      case Zxt(numbitsExpr, expr) => expr.widthExpr(id2decl) match {
        case None => Message.fatal(s"Cannot compute size of expression '${expr.toSource}' for zxt")
        case Some(widthExpr) => {
          val deltaExpr = (numbitsExpr - widthExpr).simplify
          StrList(List("{{", MakeExpr(deltaExpr), "{1'b0}},", MakeExpr(expr), "}"))
        }
      }
      case Sxt(numbitsExpr, expr) => expr.widthExpr(id2decl) match {
        case None => Message.fatal(s"Cannot compute size of expression '${expr.toSource}' for sxt")
        case Some(widthExpr) => {
          val deltaExpr = (numbitsExpr - widthExpr).simplify
          val msbExpr = expr.msbExpr(id2decl) match {
            case None       => Message.fatal(s"Cannot compute msb of expression '${expr.toSource}' for sxt")
            case Some(expr) => expr.simplify
          }
          StrList(List("{{", MakeExpr(deltaExpr), "{", MakeExpr(msbExpr), "}},", MakeExpr(expr), "}"))
        }
      }
      case DollarCall(name, args)    => StrList(List(name, "(", StrList(args.map(MakeExpr), ","), ")"))
      case ReadCall(name)            => MakeExpr(name)
      case BinaryOp(lhs, op, rhs)    => StrList(List(MakeExpr(lhs), Str(" "), op, Str(" "), MakeExpr(rhs)))
      case UnaryOp(op, lhs)          => StrList(List(op, MakeExpr(lhs)))
      case Bracket(content)          => StrList(List("(", MakeExpr(content), ")"))
      case TernaryOp(cond, lhs, rhs) => StrList(List(MakeExpr(cond), " ? ", MakeExpr(lhs), " : ", MakeExpr(rhs)))
      case BitRep(count, value)      => StrList(List("{", MakeExpr(count), "{", MakeExpr(value), "}}"))
      case BitCat(parts)             => StrList(List("{", StrList(parts.map(MakeExpr), ","), "}"))
      case DottedName(names)         => nx(names)
      case Literal(s)                => Str(s)
      case n: Num                    => n.toVerilog
      case e                         => Message.ice(s"Unexpected expression '$e'"); ""
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
    val si = Str(i)

    // Take a combinatorial statement and return stall conditions that should be emitted now based on ids that are read/written
    // This code will be emitted before the actual statement
    // It is useful to keep as a list of strings here so we can decide when to insert an extra begin/end block
    def StallExpr(tree: Node): List[String] = {
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
          case _ => Message.fatal(s"$name cannot be read"); false // TODO check this earlier?
        }
        false // No need to recurse
      }

      def v(tree: Node): Boolean = tree match {
        case CombinatorialCaseStmt(value, _, _) =>
          value visit v; false
        case CombinatorialBlock(_) => false
        case CombinatorialIf(cond, _, _) =>
          cond visit v; false
        case ReadCall(name)   => AddRead(name, false)
        case LockCall(name)   => AddRead(name, true)
        case UnlockCall(name) => AddRead(name, false)
        case WriteCall(name, _) => {
          val n: String = ExtractName(name)
          val d: Declaration = id2decl(n)
          d match {
            case OutDeclaration(fctype, _, _, stype) => {
              (fctype, stype) match {
                case (FlowControlTypeReady, StorageTypeBubble) => add(s"$go = $go && !${valid(n)};")
                case (FlowControlTypeReady, StorageTypeReg)    => add(s"$go = $go && (!${valid(n)} || ${ready(n)});")
                case (FlowControlTypeAccept, StorageTypeReg)   => Message.fatal("sync accept only supported as wire output type") // TODO check this earlier
                case (FlowControlTypeAccept, StorageTypeWire)  => add(s"$go = $go && ${accept(n)};")
                case _                                         =>
              }
              if (HasValid(fctype))
                add(s"${nx(valid(n))} = 1'b1;")
            }
            case _ => Message.fatal(s"$name cannot be written"); false // TODO check this earlier?
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

    tree match {
      case Assign(ArrayLookup(DottedName(n :: _), index :: Nil), rhs) if (Arrays contains n) => AddStall(index, rhs) {
        Str(s"""|begin
                |${i + i0}${n}_wr = 1'b1;
                |${i + i0}${n}_wraddr = ${MakeExpr(index)};
                |${i + i0}${n}_wrdata = ${MakeExpr(rhs)};
                |${i}end""".stripMargin)
      }
      case Assign(lhs, rhs) => AddStall(lhs, rhs) {
        Str(s"${MakeExpr(lhs)} = ${MakeExpr(rhs)};")
      }

      case CombinatorialBlock(cmds) => {
        // TODO remove string interpolation/mkString and use StrTree for speed
        Str(s"""|begin
                |${i + i0}${cmds map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}end""".stripMargin)
      }

      case StateBlock(_, cmds) => MakeStmt(indent)(CombinatorialBlock(cmds))

      case CombinatorialIf(cond, thenBody, optElseBody) => AddStall(cond) {
        // TODO remove string interpolation/mkString and use StrTree for speed
        val condPart = s"if (${MakeExpr(cond)}) "
        val thenPart = thenBody match {
          case block: CombinatorialBlock => MakeStmt(indent)(block)
          case other                     => MakeStmt(indent)(CombinatorialBlock(other :: Nil))
        }
        val elsePart = optElseBody match {
          case None                            => ""
          case Some(block: CombinatorialBlock) => s" else ${MakeStmt(indent)(block)}"
          case Some(other)                     => s" else ${MakeStmt(indent)(CombinatorialBlock(other :: Nil))}"
        }
        Str(condPart + thenPart + elsePart)
      }

      // TODO: do the conds not need a stall expr to be precise?
      case CombinatorialCaseStmt(value, cases, None) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseStmt(value, cases, Some(default)) => AddStall(value) {
        Str(s"""|case (${MakeExpr(value)})
                |${i + i0}${cases map MakeStmt(indent + 1) mkString s"\n${i + i0}"}
                |${i + i0}default: ${MakeStmt(indent + 1)(default)}
                |${i}endcase""".stripMargin)
      }
      case CombinatorialCaseLabel(conds, body) => Str(s"${conds map MakeExpr mkString ", "}: ${MakeStmt(indent)(body)}")

      case GotoState(target) => {
        if (numstates == 1) {
          Str("")
        } else {
          StrList(List(nx("state"), " = ", MakeState(target), ";"))
        }
      }

      case CallState(tgt, ret) => {
        Str(s"""|begin
                |${i + i0}call_stack_wr = 1'b1;
                |${i + i0}call_stack_wraddr = call_depth;
                |${i + i0}call_stack_wrdata = ${MakeState(ret)};
                |${i + i0}call_depth_nxt = call_depth + 1'b1;
                |${i + i0}${nx("state")} = ${MakeState(tgt)};
                |${i}end""".stripMargin)
        // TODO: Add assertion for call_depth_nxt < CALL_STACK_SIZE
      }

      case ReturnState => {
        Str(s"""|begin
                |${i + i0}call_depth_nxt = call_depth - 1'b1;
                |${i + i0}${nx("state")} = call_stack[call_depth_nxt];
                |${i}end""".stripMargin)
      }

      case ExprStmt(LockCall(_))   => AddStall(tree) { Str("") }
      case ExprStmt(UnlockCall(_)) => AddStall(tree) { Str("") }
      case ExprStmt(ReadCall(_))   => AddStall(tree) { Str("") }
      case ExprStmt(WriteCall(name, arg :: Nil)) => AddStall(tree) {
        Str(s"${MakeExpr(name)} = ${MakeExpr(arg)};")
      }

      case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => MakeStmt(indent)(Assign(DottedName(id :: Nil), rhs))
      case DeclarationStmt(VarDeclaration(decltype, id, None)) => MakeStmt(indent)(Assign(DottedName(id :: Nil), Num(Some(false), None, 0))) // TODO: Why is this needed ?

      case ExprStmt(DollarCall(name, args)) => StrList(List(name, "(", StrList(args.map(MakeExpr), ","), ");"))
      case AlogicComment(s) => s"// $s\n"

      case x => Message.ice(s"Don't know how to emit code for $x"); Str("")
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
    case Assign(lhs, rhs) => {
      IdsWritten.add(ExtractName(lhs))
      AddAccept(indent, AcceptExpr(lhs) ::: AcceptExpr(rhs), None)
    }
    case CombinatorialCaseStmt(value, cases, Some(default)) => {
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
    case CombinatorialIf(cond, body, Some(elsebody)) =>
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
    case CombinatorialIf(cond, body, None) => {
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

    case LockCall(name)                              => AddAccept(indent, AcceptExpr(name), None)
    case UnlockCall(name)                            => AddAccept(indent, AcceptExpr(name), None)
    case WriteCall(name, args) if (args.length == 1) => AddAccept(indent, AcceptExpr(name) ::: AcceptExpr(args(0)), None)
    case CombinatorialBlock(cmds) => {
      val s: List[Option[StrTree]] = for (c <- cmds) yield AcceptStmt(indent + 1, c)
      val s2: List[StrTree] = s.flatten
      if (s2.length == 0)
        None
      else
        Some(StrList(Str(i0 * indent) :: Str("begin\n") :: StrList(s2) :: Str(i0 * indent) :: Str("end\n") :: Nil))
    }

    case DeclarationStmt(VarDeclaration(decltype, id, Some(rhs))) => AcceptStmt(indent, Assign(DottedName(id :: Nil), rhs))
    case StateBlock(state, cmds) => {
      // Clear sets used for tracking
      syncPortsFound.clear()
      //IdsUsedToMakeAccept.clear()
      IdsWritten.clear()
      // See if there is anything to do for this state
      val s = for { cmd <- cmds } yield AcceptStmt(indent, cmd)
      val s2 = s.flatten
      if (s2.length > 0) {
        // Check for error conditions
        // TODO change sync ports into a set so only warn if different ports detected
        if (syncPortsFound.size > 1) Message.fatal(s"Cannot access multiple accept port reads in same cycle: $cmds")
        if (usesPort.isDefined) Message.fatal(s"Cannot access port $usesPort while generating accept: $cmds")
        //if (!IdsUsedToMakeAccept.intersect(IdsWritten).isEmpty) Message.warning(s"Accept is based on registered signals: check condition does not depend on a written identifier: $cmds")
        //It seems that now the state is emitted by higher level code?
        //Some(StrList(List(i0 * (indent - 1), MakeState(state), ": begin\n", StrList(s2), i0 * (indent - 1), "end\n")))
        Some(StrList(List("begin\n", StrList(s2), i0 * (indent), "end\n")))
      } else
        None
    }
    case CombinatorialCaseLabel(Nil, body) => {
      val b = AcceptStmt(indent + 1, body)
      b match {
        case None    => None
        case Some(a) => Some(StrList(Str(i0 * indent) :: Str("default:\n") :: a :: Nil))
      }
    }
    case CombinatorialCaseLabel(conds, body) => {
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
        case InDeclaration(synctype, _, _) => {
          if (HasAccept(synctype)) {
            syncPortsFound.add(n)
            add(accept(n) + " = 1'b1;\n")
          }
          if (HasReady(synctype))
            usesPort = Some(MakeExpr(name).toString)
        }
        case _ =>
      }
      false // No need to recurse
    }

    def v(tree: Node): Boolean = tree match {
      case CombinatorialCaseStmt(value, _, _) =>
        value visit v; false
      case CombinatorialBlock(_) => false
      case CombinatorialIf(cond, _, _) =>
        cond visit v; false
      case ReadCall(name)   => AddRead(name)
      case LockCall(name)   => AddRead(name)
      case UnlockCall(name) => AddRead(name)
      case WriteCall(name, _) => {
        val n: String = ExtractName(name)
        val d: Declaration = id2decl(n)
        d match {
          case OutDeclaration(synctype, _, _, _) => {
            if (HasValid(synctype))
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
