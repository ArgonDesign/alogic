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

// Handle nested fsms in network

package alogic

import scala.collection.mutable

import alogic.ast._

object MakeStages {
  def apply(net: NetworkTask): Option[(NetworkTask, List[FsmTask])] = {
    val NetworkTask(name, decls, insts, conns, vfns, fsms) = net

    val stageNames = fsms map { _.name }

    val ports = decls collect {
      case x: DeclOut => x
      case x: DeclIn  => x
    }

    // Find the pipeline variables
    val pipeVarList = decls collect { case x: DeclPippeVar => x.id -> x.kind }
    val pipeVarMapType = Map(pipeVarList: _*) // Map from name of variable to Type
    val pipeVarMap = pipeVarMapType mapValues (_.widthExpr) // Map from name of variable to expression for its width
    val pipeVarSet = pipeVarList.map(_._1).toSet // Set of names of pipeline variables

    // TODO For the moment, assume pipeline stages are instantiated in order (otherwise we should first generate a topological sort of the connections and reorder the list of fsms accordingly)
    // TODO There is a potential issue if there is not a one-to-one mapping between instantiations and fsms because different instantiations of the same unit may require different pipeline variables to be saved.
    // TODO for the moment assume there is a single instantiation of each fsm
    val firstUse = mutable.Map[String, String]() // Map from name of pipeline variable to name of fsm where that variable first is used
    val lastUse = mutable.Map[String, String]() // Map from name of pipeline variable to name of fsm where that variable last is used

    // Search each fsm for pipeline variables
    for (FsmTask(sub, decls, fns, fencefn, vfns, hasnew) <- fsms) {
      def v(tree: Node): Boolean = tree match {
        case DottedName(names) => {
          val n = names.head
          if (pipeVarSet contains n) {
            lastUse(n) = sub
            if (!firstUse.contains(n))
              firstUse(n) = sub
          }
          false
        }
        case LValName(names) => {
          val n = names.head
          if (pipeVarSet contains n) {
            lastUse(n) = sub
            if (!firstUse.contains(n))
              firstUse(n) = sub
          }
          false
        }
        case _ => true
      }
      fns.map(_ visit v)
      fencefn.map(_ visit v)
    }

    // Invert the mapping so we know for each module which variables are used first and last
    val mod2firstvars = firstUse.groupBy(_._2).mapValues(_.keys)
    val mod2lastvars = lastUse.groupBy(_._2).mapValues(_.keys)

    // Change the connections for A->B into A.p_out -> B->p_in
    var conns2 = conns map { c =>
      if (c.lhs.names.length == 1 && c.rhs.length == 1 && c.rhs.head.names.length == 1)
        Connect(DottedName(c.lhs.names.head :: "p_out" :: Nil), DottedName(c.rhs.head.names.head :: "p_in" :: Nil) :: Nil)
      else
        c
    }

    // Work out complete list of active variables for each

    // TODO this relies on toList returning the active elements in the same order for outputs and inputs - is this guaranteed?
    val activeSet = mutable.Set[String]() // Set of currently active pipeline variables
    val fsms2 = for (FsmTask(sub, decls, fns, fencefn, vfns, hasnew) <- fsms) yield {
      // Identify variables used here
      val inputs = activeSet.toList // Pipeline variables we need as inputs
      activeSet ++= mod2firstvars.getOrElse(sub, Nil)
      val used = activeSet.toList // Pipeline variables we need to declare
      activeSet --= mod2lastvars.getOrElse(sub, Nil)
      val outputs = activeSet.toList // Pipeline variables we need as outputs

      def MakeLVal(n: String) = LValName(n :: Nil)
      def MakeExpr(n: String) = DottedName(n :: Nil)

      // Adjust calls to read and write
      var usesRead = false
      var usesWrite = false
      val inName = "p_in"
      val outName = "p_out"
      val inPort = MakeExpr(inName)
      val outPort = MakeExpr(outName)
      val readCmd = Assign(LValCat(inputs map MakeLVal), ReadCall(inPort))
      val writeCmd = WriteCall(outPort, BitCat(outputs map MakeExpr) :: Nil)
      val usedPorts = mutable.Set[String]()
      def useport(name: DottedName) {
        usedPorts += name.names.head
      }
      val r: PartialFunction[Node, Node] = {
        case ExprStmt(PipelineRead) => {
          usesRead = true;
          readCmd
        }
        case PipelineWrite => {
          usesWrite = true;
          writeCmd
        }
        case x: DottedName       => { useport(x); x }
        case x @ LValName(names) => { useport(DottedName(names)); x }
      }

      val fencefn2 = fencefn.map(_ rewrite r)
      val fns2 = fns.map(_ rewrite r)

      // Add in relevant port and variable declarations for pipeline variables
      // TODO add support for spotting a naked storage type and using that for output type (once we support buffer storage type)
      var decls2 = decls
      if (usesRead)
        decls2 ::= DeclIn(IntVType(false, (inputs map pipeVarMap reduce (BinaryOp(_, "+", _))) :: Nil), inName, FlowControlTypeReady)
      if (usesWrite)
        decls2 ::= DeclOut(IntVType(false, (outputs map pipeVarMap reduce (BinaryOp(_, "+", _))) :: Nil), outName, FlowControlTypeReady, StorageTypeReg)
      if (usesRead || usesWrite)
        decls2 :::= used map { v => DeclVar(pipeVarMapType(v), v, None) }

      // Add in Decl of locally used ports
      decls2 :::= ports filter {
        case DeclIn(_, id, _) if usedPorts contains id => {
          // Add this.port -> fsm.port
          // TODO this relies on instance name matching fsm name at the moment for auto-ports
          // TODO support use of input variables even if not explicitly read
          // e.g. so network can have an "in u8 width" and the lower level modules can read this directly.
          conns2 ::= Connect(DottedName("this" :: id :: Nil), DottedName(sub :: id :: Nil) :: Nil) // TODO support several pipeline stages sharing the same input
          true
        }
        case DeclOut(_, id, _, _) if usedPorts contains id => {
          conns2 ::= Connect(DottedName(sub :: id :: Nil), DottedName("this" :: id :: Nil) :: Nil)
          true
        }
        case _ => false
      }

      val res = FsmTask(sub, decls2, fns2, fencefn2, vfns, false)
      //println(res.toSource)
      res
    }

    val stages = for (FsmTask(sub, decls, fns, fencefn, vfns, hasnew) <- fsms2) yield {
      FsmTask(s"${name}_${sub}", decls, fns, fencefn, vfns, hasnew)
    }

    val newInsts = for (inst <- insts) yield inst rewrite {
      case Instantiate(id, module, args) if stageNames contains module => Instantiate(id, s"${name}_${module}", args)
    }

    // We can also remove the pipeline declarations from the network block
    val decls2 = decls filter {
      case x: DeclPippeVar => false
      case _               => true
    }
    val network = NetworkTask(name, decls2, newInsts, conns2, vfns, Nil)

    Some((network, stages))
  }
}
