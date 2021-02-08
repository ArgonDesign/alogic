////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.ast.Trees.Expr.InstancePortSel
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Symbol
import com.argondesign.alogic.core.enums.EntityVariant
import com.argondesign.alogic.core.FlowControlTypes._
import com.argondesign.alogic.core.Types._
import com.argondesign.alogic.lib.Json
import com.argondesign.alogic.util.unreachable

import java.io.Writer
import scala.collection.immutable.ListMap
import scala.collection.mutable

object WriteSchematic extends PairsTransformerPass {
  val name = "write-schematic"

  private def writeSchematic(decl: DeclEntity, defn: DefnEntity, w: Writer): Unit = {
    val ids = Iterator.from(1).buffered

    val nodeIds = mutable.Map[Option[Symbol], Int]()
    val portIds = mutable.Map[(Option[Symbol], Symbol), Int]()

    def fcType(symbol: Symbol): FlowControlType = symbol.kind match {
      case TypeIn(_, fc)     => fc
      case TypeOut(_, fc, _) => fc
      case _                 => unreachable
    }

    def ports(iSymbolOpt: Option[Symbol], symbols: List[Symbol]): Seq[Map[String, Any]] = {
      // Inputs go on the left of a box, outputs on the right, and indices
      // are clockwise starting at the top edge, so reshuffle the ports
      // so they come out in the right order (outputs on right, in source
      // order, inputs on left in source order)
      val (outputs, inputs) = symbols.partition(_.kind.isOut)
      (outputs concat inputs.reverse).zipWithIndex.map {
        case (symbol, index) =>
          val side = symbol.kind match {
            case _: TypeOut => "EAST"
            case _: TypeIn  => "WEST"
            case _          => unreachable
          }
          portIds((iSymbolOpt, symbol)) = ids.head
          ListMap(
            "id" -> ids.next(),
            "direction" -> (if (symbol.kind.isOut) "OUTPUT" else "INPUT"),
            "hwMeta" -> ListMap(
              "level" -> 0,
              "name" -> symbol.name
            ),
            "properties" -> ListMap(
              "index" -> index,
              "side" -> side
            )
          )
      }
    }

    def portCssClass(symbol: Symbol): String = fcType(symbol) match {
      case FlowControlTypeNone  => "edge-fc-none"
      case FlowControlTypeValid => "edge-fc-valid"
      case FlowControlTypeReady => "edge-fc-ready"
    }

    def edge(
        dstInst: Option[Symbol],
        dstPort: Symbol,
        srcInst: Option[Symbol],
        srcPort: Symbol,
        loc: Loc
      ): Map[String, Any] =
      ListMap(
        "id" -> ids.next(),
        "sources" -> Seq(Seq(nodeIds(srcInst), portIds((srcInst, srcPort)))),
        "targets" -> Seq(Seq(nodeIds(dstInst), portIds((dstInst, dstPort)))),
        "hwMeta" -> ListMap(
          "name" -> loc.prefix,
          "cssClass" -> portCssClass(dstPort)
        )
      )

    def assign(iSymbolOpt: Option[Symbol], ent: EntAssign): Option[Map[String, Any]] =
      ent match {
        case EntAssign(
              InstancePortSel(lISymbol, lPSymbol),
              InstancePortSel(rISymbol, rPSymbol)
            ) =>
          Some(edge(Some(lISymbol), lPSymbol, Some(rISymbol), rPSymbol, ent.loc))
        case EntAssign(
              ExprSym(lSymbol),
              InstancePortSel(rISymbol, rPSymbol)
            ) =>
          Some(edge(iSymbolOpt, lSymbol, Some(rISymbol), rPSymbol, ent.loc))
        case EntAssign(
              InstancePortSel(lISymbol, lPSymbol),
              ExprSym(rSymbol)
            ) =>
          Some(edge(Some(lISymbol), lPSymbol, iSymbolOpt, rSymbol, ent.loc))
        case EntAssign(
              ExprSym(lSymbol),
              ExprSym(rSymbol)
            ) =>
          Some(edge(iSymbolOpt, lSymbol, iSymbolOpt, rSymbol, ent.loc))
        case _ => None // Ignore
      }

    def instances(decl: DeclEntity, defn: DefnEntity, depth: Int): Seq[Map[String, Any]] = {
      decl.decls collect {
        case decl: DeclInstance =>
          val eSymbol = decl.symbol.kind.asEntity.symbol
          entity(
            Some(decl.symbol),
            eSymbol.decl.asInstanceOf[DeclEntity],
            eSymbol.defn.asInstanceOf[DefnEntity],
            depth + 1
          )
        case DeclSram(symbol, _, size, _) =>
          ListMap(
            "id" -> ids.next(),
            "hideChildren" -> true,
            "ports" -> Seq.empty,
            "_children" -> Seq.empty,
            "_edges" -> Seq.empty,
            "hwMeta" -> ListMap[String, Any](
              "name" -> symbol.name,
              "bodyText" -> s"SRAM\\n${size}x${symbol.kind.asSram.kind.width}",
              "maxId" -> (ids.head - 1),
              "cssClass" -> s"sram"
            ),
            "properties" -> ListMap[String, Any](
              "org.eclipse.elk.layered.mergeEdges" -> 1,
              "org.eclipse.elk.portConstraints" -> "FIXED_ORDER"
            )
          )
      }
    }

    def entity(
        iSymbolOpt: Option[Symbol],
        decl: DeclEntity,
        defn: DefnEntity,
        depth: Int
      ): Map[String, Any] = {
      nodeIds(iSymbolOpt) = ids.head
      val cssClass = defn.variant match {
        case EntityVariant.Fsm => "fsm"
        case EntityVariant.Net => "net"
        case EntityVariant.Ver => "ver"
        case _                 => unreachable // Broken enums
      }
      ListMap(
        "id" -> ids.next(),
        "hideChildren" -> true,
        "ports" -> ports(iSymbolOpt, decl.ports),
        "_children" -> instances(decl, defn, depth),
        "_edges" -> defn.assigns.flatMap(assign(iSymbolOpt, _)),
        "hwMeta" -> ListMap[String, Any](
          "name" -> iSymbolOpt.map(_.name).getOrElse("TOP"),
          "bodyText" -> decl.symbol.name,
          "maxId" -> (ids.head - 1),
          "cssClass" -> s"entity $cssClass ${if (depth % 2 == 0) "even" else "odd"}"
        ),
        "properties" -> ListMap[String, Any](
          "org.eclipse.elk.layered.mergeEdges" -> 1,
          "org.eclipse.elk.portConstraints" -> (if (iSymbolOpt.isEmpty) "FIXED_ORDER"
                                                else "FIXED_SIDE"),
          "org.eclipse.elk.layered.cycleBreaking.strategy" -> "DEPTH_FIRST",
          "org.eclipse.elk.layered.compaction.connectedComponents" -> true
        )
      )
    }

    val hierarchy = entity(None, decl, defn, 0)

    val (topLevelPorts, topLevelEdges) = decl.ports.map { symbol =>
      val topNode = ids.next()
      val topPortId = ids.next()
      val node = ListMap[String, Any](
        "id" -> topNode,
        "hideChildren" -> true,
        "ports" -> Seq(
          ListMap(
            "id" -> topPortId,
            "direction" -> (if (symbol.kind.isIn) "OUTPUT" else "INPUT"),
            "hwMeta" -> ListMap[String, Any](
              "level" -> 0,
              "name" -> symbol.name
            ),
            "properties" -> ListMap[String, Any](
              "index" -> 0,
              "side" -> (if (symbol.kind.isIn) "EAST" else "WEST")
            )
          )
        ),
        "_children" -> Seq.empty,
        "_edges" -> Seq.empty,
        "hwMeta" -> ListMap[String, Any](
          "name" -> symbol.name,
          "isExternalPort" -> true,
          "maxId" -> (ids.head - 1)
        ),
        "properties" -> ListMap[String, Any](
          "org.eclipse.elk.layered.mergeEdges" -> 1,
          "org.eclipse.elk.portConstraints" -> "FIXED_ORDER"
        )
      )

      val edge = {
        val extRef = Seq(Seq(topNode, topPortId))
        val topRef = Seq(Seq(nodeIds(None), portIds((None, symbol))))
        ListMap[String, Any](
          "id" -> ids.next(),
          "sources" -> (if (symbol.kind.isIn) extRef else topRef),
          "targets" -> (if (symbol.kind.isIn) topRef else extRef),
          "hwMeta" -> ListMap(
            "name" -> "top level port",
            "cssClass" -> portCssClass(symbol)
          )
        )
      }

      (node, edge)
    }.unzip

    val graph = ListMap[String, Any](
      "hideChildren" -> false,
      "ports" -> Seq.empty,
      "children" -> (hierarchy :: topLevelPorts),
      "edges" -> topLevelEdges,
      "hwMeta" -> ListMap(
        "maxId" -> (ids.head - 1)
      ),
      "properties" -> ListMap[String, Any](
        "org.eclipse.elk.layered.mergeEdges" -> 1,
        "org.eclipse.elk.portConstraints" -> "FIXED_ORDER"
      )
    )

    // Write header
    w.write(
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |  <meta charset="utf-8">
         |  <meta name="viewport" content="width=device-width, height=device-height, initial-scale=1">
         |  <title>Schematic of ${decl.symbol.name}</title>
         |  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/d3-hwschematic@0.1.5/dist/d3-hwschematic.css" integrity="sha256-9qJhx5mGY+wqIHA7qpM049zemDyNP5FotgINgnCQF1U=" crossorigin="anonymous">
         |  <style>
         |    html, body {
         |      width: 100%;
         |      height: 100%;
         |    }
         |    body {
         |      overflow: hidden;
         |    }
         |    .node rect {
         |      stroke-width: 2;
         |      rx: 0;
         |      ry: 0;
         |    }
         |    .d3-hwschematic .node {
         |      stroke: #000000;
         |      stroke-opacity: 1;
         |    }
         |    .d3-hwschematic .node.entity.fsm {
         |      fill: #e0ffe0;
         |    }
         |    .d3-hwschematic .node.entity.net.even {
         |      fill: #e0e0ff;
         |    }
         |    .d3-hwschematic .node.entity.net.odd {
         |      fill: #c0c0ff;
         |    }
         |    .d3-hwschematic .node.entity.ver {
         |      fill: #e0e0e0;
         |    }
         |    .d3-hwschematic .node.sram {
         |      fill: #ffe0e0;
         |    }
         |    .d3-hwschematic .edge-fc-none {
         |      stroke: transparent;
         |    }
         |    .d3-hwschematic .edge-fc-valid {
         |      stroke: #a0e0ff;
         |    }
         |    .d3-hwschematic .edge-fc-ready {
         |      stroke: #a0ffa0;
         |    }
         |    .d3-hwschematic .edge-fc-none:not(.link-wrap-activated) {
         |      stroke-width: 4;
         |    }
         |    .d3-hwschematic .edge-fc-valid:not(.link-wrap-activated) {
         |      stroke-width: 4;
         |    }
         |    .d3-hwschematic .edge-fc-ready:not(.link-wrap-activated) {
         |      stroke-width: 4;
         |    }
         |    .d3-hwschematic .link {
         |      stroke: #e0e0e0
         |      stroke-opacity: 1;
         |    }
         |    .d3-hwschematic .link-selected {
         |      stroke-width: 8;
         |    }
         |  </style>
         |  <script src="https://cdn.jsdelivr.net/npm/d3@6.5.0/dist/d3.min.js" integrity="sha256-Mv/fw5ENeWCPouQaykcRP0HjXyRi+HgWDZC4Nr8pfSM=" crossorigin="anonymous"></script>
         |  <script src="https://cdn.jsdelivr.net/npm/elkjs@0.7.1/lib/elk.bundled.js" integrity="sha256-ZvQe6swp1Zzo1rQNMswqVRmp3AdSl+CcW2NE3Jh15gI=" crossorigin="anonymous"></script>
         |  <script src="https://cdn.jsdelivr.net/npm/d3-hwschematic@0.1.5/dist/d3-hwschematic.min.js"></script>
         |</head>
         |<body>
         |  <svg id="scheme-placeholder"></svg>
         |  <script>
         |     var graph = """.stripMargin
    )

    // Write graph
    Json.write(w, graph)

    // Write footer
    w.write("""  </script>
              |  <script>
              |    var svg = d3.select("#scheme-placeholder")
              |        .attr("width", document.body.clientWidth)
              |        .attr("height", document.body.clientHeight);
              |
              |    document.body.onresize = function() {
              |        svg.attr("width", document.body.clientWidth);
              |        svg.attr("height", document.body.clientHeight);
              |    }
              |
              |    var hwSchematic = new d3.HwSchematic(svg);
              |    var zoom = d3.zoom();
              |    zoom.on("zoom", function applyTransform(ev) {
              |        hwSchematic.root.attr("transform", ev.transform)
              |    });
              |
              |    // disable zoom on doubleclick
              |    // because it interferes with component expanding/collapsing
              |    svg.call(zoom)
              |       .on("dblclick.zoom", null)
              |
              |    hwSchematic.bindData(graph);
              |  </script>
              |</body>
              |</html>
              |""".stripMargin)

  }

  def process(input: Pairs)(implicit cc: CompilerContext): Pairs = {
    if (cc.settings.schematic) {
      // Write a schematic for each top level entity
      input.asPar
        .collect {
          case (decl: DeclEntity, defn: DefnEntity) if decl.symbol.attr.topLevel.isSet =>
            (decl, defn)
        }
        .foreach {
          case (decl, defn) =>
            val w = cc.getOutputWriter(s"schematic-${decl.symbol.name}.html")
            writeSchematic(decl, defn, w)
            w.close()
        }
    }

    // No actual transformations
    input
  }

}
