////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Compilation test framework
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File
import java.io.PrintWriter
import java.io.StringWriter
import java.io.Writer
import java.nio.file.Files
import java.nio.file.Path

import com.argondesign.alogic.ast.Trees.Entity
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.Settings
import org.scalatest.FreeSpecLike
import org.scalatest.ParallelTestExecution

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

trait CompilationTest extends FreeSpecLike with AlogicTest with ParallelTestExecution {

  val verilatorPath = new File("verilator/install/bin/verilator")

  if (!verilatorPath.exists) {
    cancel("Run the 'setup-verilator' script to install Verilator locally for testing")
  }

  // Will be updated form multiple threads so must be thread safe
  val outputs = scala.collection.concurrent.TrieMap[String, String]()

  // Insert compiler output to the 'outputs' map above
  def entityWriterFactory(entity: Entity, suffix: String): Writer = {
    new StringWriter {
      override def close(): Unit = {
        outputs(entity.name + suffix) = this.toString
        super.close()
      }
    }
  }

  // Create temporary directory, run function passing the path to the temporary
  // directory as argument, then remove the temporary directory
  private def withTmpDir[R](f: Path => R): R = {
    val tmpDir = Files.createTempDirectory("alogic-test-")
    try {
      f(tmpDir)
    } finally {
      def del(f: File): Unit = {
        if (f.isDirectory) {
          f.listFiles foreach del
        }
        f.delete()
      }
      del(tmpDir.toFile)
    }
  }

  // Lint compiler output with verilator
  private def verilatorLint(topLevel: String): Unit = withTmpDir { tmpDir =>
    // Write all files to temporary directory
    for ((name, text) <- outputs) {
      val fullPath = tmpDir.resolve(name).toFile
      val pw = new PrintWriter(fullPath)
      pw.write(text)
      pw.flush()
      pw.close()
    }

    // Lint the top level
    val ret = s"${verilatorPath} --lint-only -Wall -y ${tmpDir} ${topLevel}".!

    // Fail test if lint failed
    if (ret != 0) {
      fail("Verilator lint error")
    }
  }

  def parseCheckFile(checkFile: String): (Map[String, String], List[Message]) = {
    val attr = mutable.Map[String, String]()
    val pairMatcher = """@(.*):(.*)""".r
    val boolMatcher = """@(.*)""".r
    for {
      line <- Source.fromFile(checkFile).getLines map { _.trim }
      if line startsWith "//"
    } {
      line.drop(2).trim match {
        case pairMatcher(k, v) => attr(k.trim) = v.trim
        case boolMatcher(k)    => attr(k.trim) = ""
        case _                 =>
      }
    }
    (attr.toMap, Nil)
  }

  def defineTest(name: String, searchPath: File, top: String, checkFile: String): Unit = {
    name in {
      // Create compiler context
      val cc: CompilerContext = new CompilerContext(
        Settings(
          moduleSearchDirs = List(searchPath),
          entityWriterFactory = entityWriterFactory,
          dumpTrees = false
        )
      )

      // Do the compilation
      cc.compile(List(top))

      // Parse the check file
      val (attr, _) = parseCheckFile(checkFile)

      // Lint
      if (!cc.hasError && !(attr contains "verilator-lint-off")) {
        verilatorLint(attr.getOrElse("out-top", top))
      }

      // Check
      cc.messages foreach println
      cc.hasError shouldBe false
    }
  }
}
