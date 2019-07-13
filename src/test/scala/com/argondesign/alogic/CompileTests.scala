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
// Compile single alogic files as tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File
import java.io.StringWriter
import java.io.Writer

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Settings
import org.scalatest.FreeSpec
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer

final class CompileTests extends FreeSpec with Matchers {

  def fixture(path: File): (ListBuffer[(String, String)], CompilerContext) = {
    val outputs = ListBuffer[(String, String)]()

    def entityWriterFactory(entity: Entity, suffix: String): Writer = {
      new StringWriter {
        override def close(): Unit = {
          outputs += (entity.name + suffix) -> this.toString
          super.close()
        }
      }
    }

    val cc = new CompilerContext(
      Settings(
        moduleSearchDirs = List(path),
        entityWriterFactory = entityWriterFactory,
        dumpTrees = true
      )
    )

    (outputs, cc)
  }

  def dump(cc: CompilerContext, outputs: List[(String, String)]) = {
    val printVerilog = false
    outputs foreach {
      case (k, v) if cc.hasError || printVerilog && (k endsWith ".v") =>
        println("#" * 80)
        println(k)
        println(v)
      case _ => ()
    }
    cc.messages foreach println
  }

  "These source files should compile on their own" - {
    val base = "/compile/single"

    val path = new File(getClass.getResource(base).getPath)

    val sources = (path.listFiles filter { f =>
      f.isFile && f.getName.endsWith(".alogic")
    }).sorted.toList

    val pattern = """.*/(.*)\.alogic$""".r

    sources foreach { s =>
      pattern.findAllIn(s.getPath).matchData foreach { m =>
        val top = m.group(1)
        s"${base.tail}/${top}.alogic" in {
          val (outputs, cc) = fixture(path)
          try {
            cc.compile(List(top))
          } finally {
            dump(cc, outputs.toList)
          }
          cc.hasError shouldBe false
        }
      }
    }
  }

  "These directories should compile as a design" - {
    val base = "/compile/multi"

    val path = new File(getClass.getResource(base).getPath)

    val dirs = (path.listFiles filter { _.isDirectory }).sorted.toList

    dirs foreach { dir =>
      s"${base.tail}/${dir.getName}" in {
        val (outputs, cc) = fixture(dir)
        try {
          cc.compile(List("top"))
        } finally {
          dump(cc, outputs.toList)
        }
        cc.hasError shouldBe false
      }
    }
  }
}
