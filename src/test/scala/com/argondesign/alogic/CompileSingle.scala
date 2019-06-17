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
import com.argondesign.alogic.ast.Trees.EntityIdent
import com.argondesign.alogic.ast.Trees.EntityLowered
import com.argondesign.alogic.ast.Trees.EntityNamed
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Settings
import org.scalatest.FreeSpec
import org.scalatest.Matchers

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

final class CompileSingle extends FreeSpec with Matchers {

  val base = "/compile/single"

  val path = new File(getClass.getResource(base).getPath)

  def fixture(): (ListBuffer[(String, String)], CompilerContext) = {
    val outputs = ListBuffer[(String, String)]()

    def entityWriterFactory(entity: Entity, suffix: String): Writer = {
      new StringWriter {
        override def close(): Unit = {
          val name = entity match {
            case e: EntityLowered => e.symbol.name
            case e: EntityNamed   => e.symbol.name
            case e: EntityIdent   => e.ident.name
          }

          outputs += (name + suffix) -> this.toString
          super.close()
        }
      }
    }

    val cc = new CompilerContext(
      Settings(
        moduleSearchDirs = List(path),
        entityWriterFactory = entityWriterFactory,
        dumpTrees = false
      )
    )

    (outputs, cc)
  }

  val sources = (path.listFiles filter { f =>
    f.isFile && f.getName.endsWith(".alogic")
  }).sorted.toList

  val pattern = """.*/(.*)\.alogic$""".r

  "These source files should compile on their own" - {
    sources foreach { s =>
      pattern.findAllIn(s.getPath).matchData foreach { m =>
        val top = m.group(1)
        s"${base.tail}/${top}.alogic" in {
          val (outputs, cc) = fixture()
          try {
            cc.compile(List(top))
          } finally {
            if (cc.hasError) {
              outputs.toList foreach {
                case (k, v) =>
                  println("#" * 80)
                  println(k)
                  println(v)
              }
            }
            cc.messages foreach println
          }
          cc.hasError shouldBe false
        }
      }
    }
  }
}
