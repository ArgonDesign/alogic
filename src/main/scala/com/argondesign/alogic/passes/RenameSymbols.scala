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

import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Messages.Ice
import com.argondesign.alogic.core.Symbols.Symbol
import com.argondesign.alogic.util.SequenceNumbers

import scala.annotation.tailrec
import scala.collection.Iterable
import scala.collection.mutable

object RenameSymbols {

  // format: off
  // IEEE 1800-2017 Annex B
  private val systemVerilogKeywords = Set(
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
    "while", "wildcard", "wire", "with", "within", "wor", "xnor", "xor"
  )
  // format: on

  def makeNamesUnique(symbols: Iterable[Symbol])(implicit cc: CompilerContext): Unit =
    symbols.groupBy(_.name).iterator filter { _._2.sizeIs > 1 } foreach {
      case (name, symbols) =>
        // Group by line
        val groupedByLine = symbols.groupBy(_.loc.line)
        // Only add line number if there are definitions on multiple lines
        val addLineNumber = groupedByLine.sizeIs > 1
        // Rename symbols
        groupedByLine foreach {
          case (line, symbols) =>
            val nameLine = if (addLineNumber) s"$name${cc.sep}l$line" else name
            if (symbols.sizeIs == 1) {
              // Only one symbol on this line, line number will disambiguate
              symbols.head.name = nameLine
            } else {
              // Ensure uniqueness, even if defined on the same line
              val seq = new SequenceNumbers
              symbols foreach { _.name = s"$nameLine${cc.sep}${seq.next}" }
            }
        }
    }

  def apply(last: Boolean): PairsTransformerPass =
    new PairsTransformerPass {
      val name = "rename-symbols"

      override protected def process(
          input: List[(Decl, Defn)]
        )(
          implicit
          cc: CompilerContext
        ): List[(Decl, Defn)] = {

        val topNames = mutable.Set[String]()

        def fixIfKeyword(name: String): String = {
          if (systemVerilogKeywords contains name) {
            name + "_" // Add a single underscore
          } else {
            name
          }
        }

        // Rename symbol within entities that have the same name
        input.iterator collect {
          case (decl: DeclEntity, _) => decl
        } foreach {
          case DeclEntity(eSymbol, decls) =>
            makeNamesUnique(decls map { _.symbol })

            // Only run on very last rename pass (just before code generation)
            if (last) {
              // Fix keywords
              decls foreach { decl =>
                decl.symbol.name = fixIfKeyword(decl.symbol.name)
              }

              // Add entity prefix
              val ep = cc.settings.ensurePrefix
              val prefix = (0 to ep.length) collectFirst {
                case n if eSymbol.name startsWith ep.drop(n) => ep.take(n)
              }

              // Also ensure not keyword
              val nameWithPrefix = fixIfKeyword(prefix.get + eSymbol.name)

              // Enforce max name length if provided
              val entityNameWithLegalLen = cc.settings.outputNameMaxLength match {
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

              eSymbol.name = entityNameWithLegalLen

              if (topNames(eSymbol.name)) {
                throw Ice(eSymbol.loc, "entity name collision")
              }

              topNames += eSymbol.name
            }
        }

        input
      }

    }

}
