////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Make symbol names unique, and rename them if necessary.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.ast.StatelessTreeTransformer
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.core.Types.TypeEntity
import com.argondesign.alogic.typer.TypeAssigner
import com.argondesign.alogic.util.SequenceNumbers

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.Iterable
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

object RenameSymbols {

  // We rename all symbols that are target language keywords, or keywords
  // in related languages (HDL linters usually warn for using keywords of
  // other HDLs as identifiers because this hinders interoperability when
  // using mixed sources is a design). The following Set is constructed
  // lazily, once per compiler process, so we leave duplicates in the
  // initializer in exchange for simpler maintenance.
  // format: off
  private lazy val keywords = Set(
    // SystemVerilog keywords from: IEEE 1800-2017 Annex B
    "accept_on", "alias", "always", "always_comb", "always_ff", "always_latch",
    "and", "assert", "assign", "assume", "automatic", "before", "begin", "bind",
    "bins", "binsof", "bit", "break", "buf", "bufif0", "bufif1", "byte", "case",
    "casex", "casez", "cell", "chandle", "checker", "class", "clocking", "cmos",
    "config", "const", "constraint", "context", "continue", "cover",
    "covergroup", "coverpoint", "cross", "deassign", "default", "defparam",
    "design", "disable", "dist", "do", "edge", "else", "end", "endcase",
    "endchecker", "endclass", "endclocking", "endconfig", "endfunction",
    "endgenerate", "endgroup", "endinterface", "endmodule", "endpackage",
    "endprimitive", "endprogram", "endproperty", "endsequence", "endspecify",
    "endtable", "endtask", "enum", "event", "eventually", "expect", "export",
    "extends", "extern", "final", "first_match", "for", "force", "foreach",
    "forever", "fork", "forkjoin", "function", "generate", "genvar", "global",
    "highz0", "highz1", "if", "iff", "ifnone", "ignore_bins", "illegal_bins",
    "implements", "implies", "import", "incdir", "include", "initial", "inout",
    "input", "inside", "instance", "int", "integer", "interconnect",
    "interface", "intersect", "join", "join_any", "join_none", "large", "let",
    "liblist", "library", "local", "localparam", "logic", "longint",
    "macromodule", "matches", "medium", "modport", "module", "nand", "negedge",
    "nettype", "new", "nexttime", "nmos", "nor", "noshowcancelled", "not",
    "notif0", "notif1", "null", "or", "output", "package", "packed",
    "parameter", "pmos", "posedge", "primitive", "priority", "program",
    "property", "protected", "pull0", "pull1", "pulldown", "pullup",
    "pulsestyle_ondetect", "pulsestyle_onevent", "pure", "rand", "randc",
    "randcase", "randsequence", "rcmos", "real", "realtime", "ref", "reg",
    "reject_on", "release", "repeat", "restrict", "return", "rnmos", "rpmos",
    "rtran", "rtranif0", "rtranif1", "s_always", "scalared", "sequence",
    "s_eventually", "shortint", "shortreal", "showcancelled", "signed", "small",
    "s_nexttime", "soft", "solve", "specify", "specparam", "static", "string",
    "strong", "strong0", "strong1", "struct", "s_until", "s_until_with",
    "super", "supply0", "supply1", "sync_accept_on", "sync_reject_on", "table",
    "tagged", "task", "this", "throughout", "time", "timeprecision", "timeunit",
    "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand", "trior",
    "trireg", "type", "typedef", "union", "unique", "unique0", "unsigned",
    "until", "until_with", "untyped", "use", "uwire", "var", "vectored",
    "virtual", "void", "wait", "wait_order", "wand", "weak", "weak0", "weak1",
    "while", "wildcard", "wire", "with", "within", "wor", "xnor", "xor",

    // VHDL keywords from: IEEE 1076-2019 section 15.10
    "abs", "access", "after", "alias", "all", "and", "architecture", "array",
    "assert", "assume", "attribute", "begin", "block", "body", "buffer", "bus",
    "case", "component", "configuration", "constant", "context", "cover",
    "default", "disconnect", "downto", "else", "elsif", "end", "entity", "exit",
    "fairness", "file", "for", "force", "function", "generate", "generic",
    "group", "guarded", "if", "impure", "in", "inertial", "inout", "is",
    "label", "library", "linkage", "literal", "loop", "map", "mod", "nand",
    "new", "next", "nor", "not", "null", "of", "on", "open", "or", "others",
    "out", "package", "parameter", "port", "postponed", "procedure", "process",
    "property", "protected", "private", "pure", "range", "record", "register",
    "reject", "release", "rem", "report", "restrict", "return", "rol", "ror",
    "select", "sequence", "severity", "signal", "shared", "sla", "sll", "sra",
    "srl", "strong", "subtype", "then", "to", "transport", "type", "unaffected",
    "units", "until", "use", "variable", "view", "vpkg", "vmode", "vprop",
    "vunit", "wait", "when", "while", "with", "xnor", "xor",

    // Verilog-AMS keywords from: Verilog-AMS LRM 2.4.0 Annex B
    "above", "abs", "absdelay", "absdelta", "abstol", "access", "acos", "acosh",
    "ac_stim", "aliasparam", "always", "analog", "analysis", "and", "asin",
    "asinh", "assert", "assign", "atan", "atan2", "atanh", "automatic", "begin",
    "branch", "buf", "bufif0", "bufif1", "case", "casex", "casez", "ceil",
    "cell", "cmos", "config", "connect", "connectmodule", "connectrules",
    "continuous", "cos", "cosh", "cross", "ddt", "ddt_nature", "ddx",
    "deassign", "default", "defparam", "design", "disable", "discipline",
    "discrete", "domain", "driver_update", "edge", "else", "end", "endcase",
    "endconfig", "endconnectrules", "enddiscipline", "endfunction",
    "endgenerate", "endmodule", "endnature", "endparamset", "endprimitive",
    "endspecify", "endtable", "endtask", "event", "exclude", "exp",
    "final_step", "flicker_noise", "floor", "flow", "for", "force", "forever",
    "fork", "from", "function", "generate", "genvar", "ground", "highz0",
    "highz1", "hypot", "idt", "idtmod", "idt_nature", "if", "ifnone", "incdir",
    "include", "inf", "initial", "initial_step", "inout", "input", "instance",
    "integer", "join", "laplace_nd", "laplace_np", "laplace_zd", "laplace_zp",
    "large", "last_crossing", "liblist", "library", "limexp", "ln",
    "localparam", "log", "macromodule", "max", "medium", "merged", "min",
    "module", "nand", "nature", "negedge", "net_resolution", "nmos",
    "noise_table", "noise_table_log", "nor", "noshowcancelled", "not", "notif0",
    "notif1", "or", "output", "parameter", "paramset", "pmos", "posedge",
    "potential", "pow", "primitive", "pull0", "pull1", "pulldown", "pullup",
    "pulsestyle_onevent", "pulsestyle_ondetect", "rcmos", "real", "realtime",
    "reg", "release", "repeat", "resolveto", "rnmos", "rpmos", "rtran",
    "rtranif0", "rtranif1", "scalared", "sin", "sinh", "showcancelled",
    "signed", "slew", "small", "specify", "specparam", "split", "sqrt",
    "string", "strong0", "strong1", "supply0", "supply1", "table", "tan",
    "tanh", "task", "time", "timer", "tran", "tranif0", "tranif1", "transition",
    "tri", "tri0", "tri1", "triand", "trior", "trireg", "units", "unsigned",
    "use", "uwire", "vectored", "wait", "wand", "weak0", "weak1", "while",
    "white_noise", "wire", "wor", "wreal", "xnor", "xor", "zi_nd", "zi_np",
    "zi_zd", "zi_zp"
  )
  // format: on

  def makeNamesUnique(
      symbols: Iterable[Symbol],
      ignore: Set[Symbol] = Set.empty
    )(
      implicit
      cc: CompilerContext
    ): Unit =
    symbols.groupBy(_.name).iterator filter { _._2.sizeIs > 1 } foreach {
      case (name, symbols) =>
        // Group by line
        val groupedByLine = symbols.groupBy(_.loc.line)
        // Only add line numbers if there are definitions on multiple lines
        val addLineNumber = groupedByLine.sizeIs > 1
        // Rename symbols
        groupedByLine.iterator map {
          case (line, symbols) =>
            (if (addLineNumber) s"$name${cc.sep}l$line" else name, symbols filterNot ignore)
        } foreach {
          case (base, symbols) =>
            if (symbols.sizeIs == 1) {
              // Only one symbol on this line, line number will disambiguate
              symbols.head.name = base
            } else {
              // Ensure uniqueness, even if defined on the same line
              val seq = new SequenceNumbers
              symbols foreach { _.name = s"$base${cc.sep}${seq.next}" }
            }
        }
    }

  private def fixIfKeyword(name: String): String = if (keywords contains name) {
    name + "_" // Add a single underscore
  } else {
    name
  }

  def apply(last: Boolean): PairsTransformerPass =
    new PairsTransformerPass {
      val name = "rename-symbols"

      override protected def process(
          input: Iterable[(Decl, Defn)]
        )(
          implicit
          cc: CompilerContext
        ): Iterable[(Decl, Defn)] = {

        // Set of top level names to avoid collissions
        val topNames = mutable.Set[String]()

        // Map from (entitySymbol, publicSymbolOldName) -> publicSymbolNewName
        val publicNameMap = TrieMap[(Symbol, String), String]()

        // Process each Entity
        input.par foreach {
          case (DeclEntity(eSymbol, decls), _) =>
            ////////////////////////////////////////////////////////////////////
            // Rename the member symbols
            ////////////////////////////////////////////////////////////////////

            val publicSymbols = Set.from(eSymbol.kind.asType.kind.asEntity.publicSymbols)

            val symbols = decls map { _.symbol }

            // Rename symbols within entities that have the same name
            makeNamesUnique(symbols, publicSymbols)

            // Only run on very last rename pass (just before code generation)
            if (last) {
              // Rename symbols with names that are target language keywords
              symbols foreach { symbol =>
                val newName = fixIfKeyword(symbol.name)
                // Memorize mapping of renamed public symbols
                if (publicSymbols(symbol) && newName != symbol.name) {
                  publicNameMap((eSymbol, symbol.name)) = newName
                }
                symbol.name = newName
              }
            }

            ////////////////////////////////////////////////////////////////////
            // Rename the entity
            ////////////////////////////////////////////////////////////////////

            // Only run on very last rename pass (just before code generation)
            if (last) {
              // Add entity prefix if provided
              val ep = cc.settings.ensurePrefix
              val prefix = (0 to ep.length) collectFirst {
                case n if eSymbol.name startsWith ep.drop(n) => ep.take(n)
              }

              // Also ensure the entity name is not keyword
              val nameWithPrefix = fixIfKeyword(prefix.get + eSymbol.name)

              // Enforce max name length if provided
              val nameWithLegalLen = cc.settings.outputNameMaxLength match {
                case Some(limit) if nameWithPrefix.length > limit && !eSymbol.attr.topLevel.isSet =>
                  val (nameKeep, nameDrop) = nameWithPrefix.splitAt(limit - 16)

                  @tailrec
                  def generateUniqueShortName(salt: Int): String = {
                    val shortName = {
                      val salted = salt.toString + nameDrop
                      val md5Buf =
                        java.security.MessageDigest.getInstance("MD5").digest(salted.getBytes)
                      val md5Str = md5Buf take 7 map {
                        "%02x" format _
                      } mkString ""
                      s"${nameKeep}_h$md5Str"
                    }

                    // Try again on hash collision
                    if (topNames(shortName)) generateUniqueShortName(salt + 1) else shortName
                  }

                  generateUniqueShortName(0)
                case _ => nameWithPrefix
              }

              // Finally rename the symbol, check consistency, and remember name
              eSymbol.name = nameWithLegalLen
              assert(!topNames(eSymbol.name), "entity name collision")
              topNames addOne eSymbol.name
            }

          //
          case _ =>
        }

        // If we renamed public symbols, fix select references
        if (publicNameMap.isEmpty) {
          input
        } else {
          assert(last, "Should only rename public symbols on last rename pass")
          object Transform extends StatelessTreeTransformer {
            override protected def transform(tree: Tree): Tree = tree match {
              case expr @ ExprSel(tgt, selector, _) =>
                tgt.tpe match {
                  case TypeEntity(eSymbol, _) =>
                    publicNameMap.get((eSymbol, selector)) map { newSelector =>
                      TypeAssigner(expr.copy(selector = newSelector) withLoc tree.loc)
                    } getOrElse tree
                  case _ => tree
                }
              case _ => tree
            }
          }

          (input.par map { case (decl, defn) => (decl, defn rewrite Transform) }).seq
        }
      }

    }

}
