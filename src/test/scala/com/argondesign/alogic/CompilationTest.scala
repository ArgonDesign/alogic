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

import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages.Error
import com.argondesign.alogic.core.Messages.Fatal
import com.argondesign.alogic.core.Messages.Message
import com.argondesign.alogic.core.Messages.Note
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable
import io.circe.Json
import org.scalatest.ConfigMap
import org.scalatest.ParallelTestExecution
import org.scalatest.fixture
import org.scalatest.freespec.FixtureAnyFreeSpec
import org.scalatest.Tag

import java.io.File
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.AnsiColor
import scala.sys.process._

trait CompilationTest
    extends FixtureAnyFreeSpec
    with AlogicTest
    with fixture.ConfigMapFixture
    with ParallelTestExecution {

  private def getTool(path: Path, name: String, msg: String): File = {
    path.resolve(name).toFile
  } pipe { _.getCanonicalFile } ensuring (_.exists, msg)

  private lazy val verilatorPath = Paths.get("verilator/install/bin").toAbsolutePath

  private lazy val verilator = getTool(
    verilatorPath,
    "verilator",
    "Run the 'setup-verilator' script to install Verilator locally for testing"
  )

  private lazy val symbiyosysPath = Paths.get("symbiyosys/install/bin").toAbsolutePath

  private lazy val symbiyosys = getTool(
    symbiyosysPath,
    "sby",
    "Run the 'setup-symbiyosys' script to install SymbiYosys locally for testing"
  )

  private lazy val yosys = getTool(
    symbiyosysPath,
    "yosys",
    "Run the 'setup-symbiyosys' script to install SymbiYosys locally for testing"
  )

  // Test config
  private case class Config(
      trace: Boolean,
      verbose: Int)

  // Create temporary directory, run function passing the path to the temporary
  // directory as argument, then remove the temporary directory
  private def withTmpDir(tmpDir: Option[String])(f: Path => Unit): Unit = {
    def del(f: File): Unit = {
      if (f.isDirectory) {
        f.listFiles foreach del
      }
      f.delete()
    }
    tmpDir match {
      case Some(tmpDir) =>
        val tmp = new File(tmpDir)
        if (tmp.exists) {
          del(tmp)
        }
        tmp.mkdirs()
        f(tmp.toPath.toAbsolutePath)
      case None =>
        val tmpPath = Files.createTempDirectory("alogic-test-").toAbsolutePath
        try {
          f(tmpPath)
        } finally {
          del(tmpPath.toFile)
        }
    }
  }

  private def writeFile(file: File)(content: String): Unit = {
    val pw = new PrintWriter(file)
    pw.write(content)
    pw.flush()
    pw.close()
  }

  private def system(
      cmd: String,
      cwd: Path,
      logfile: String,
      failOk: Boolean = false,
      extraEnv: Map[String, String] = Map.empty
    )(
      implicit
      config: Config
    ): Boolean = {
    val logFile = cwd.resolve(logfile).toFile
    val logger = ProcessLogger(logFile)
    if (config.verbose > 0) {
      println(s"${AnsiColor.BOLD}=== Running in directory: $cwd${AnsiColor.RESET}")
      println(cmd)
    }
    val ret = Process(cmd, cwd.toFile, extraEnv.toSeq: _*) ! logger
    logger.flush()
    logger.close()
    if (config.verbose > 0) {
      println("=== Output:")
      Source(logFile).linesIterator foreach println
    }
    if (!failOk && ret != 0) {
      fail(s"Command failed: '$cmd'")
    }
    ret == 0
  }

  private def allMatches(patterns: Iterable[String], lines: Iterable[String]): Boolean = {
    lines.sizeIs == patterns.size && { // Right length
      (patterns.iterator zip lines.iterator) forall { // Right content
        case (pattern, line) => pattern.r.pattern.matcher(line).matches()
      }
    }
  }

  // Lint compiler output with Verilator
  private def verilatorLint(topLevel: String, oPath: Path)(implicit config: Config): Unit = {
    // Lint the top level
    system(s"$verilator --lint-only -Wall -y $oPath $topLevel", oPath, "verilator-lint.log")
  }

  // Build and run simulation with Verilator
  private def verilatorSim(
      topLevel: String,
      test: String,
      expect: String,
      dpi: String,
      timeout: Long,
      trace: Boolean,
      oPath: Path,
      manifest: Json
    )(
      implicit
      config: Config
    ): Unit = {

    val topLevelManifest = manifest.hcursor.downField("top-levels").downField(topLevel)

    // TODO: factor common with fec
    val clock = topLevelManifest.get[String]("clock").toOption.getOrElse("clk")
    val reset = topLevelManifest.get[String]("reset").toOption.getOrElse("rst")

    val resetStyle = topLevelManifest.get[String]("reset-style") match {
      case Right("sync-high")  => ResetStyle.SyncHigh
      case Right("sync-low")   => ResetStyle.SyncLow
      case Right("async-high") => ResetStyle.AsyncHigh
      case Right("async-low")  => ResetStyle.AsyncLow
      case other               => fail("Unknown reset style: " + other)
    }

    // Write the testbench
    val tbFile = oPath.resolve("testbench.sv").toFile
    assert(!tbFile.exists)
    writeFile(tbFile) {
      s"""module testbench(
         |  input wire $clock,
         |  input wire $reset
         |);
         |
         |$test
         |
         |  $topLevel dut (.*);
         |
         |endmodule
         |""".stripMargin
    }

    // Write the dpi source file
    val dpiFileOpt = Option.when(dpi.nonEmpty)(oPath.resolve("dpi.cc").toFile)
    dpiFileOpt foreach { dpiFile =>
      writeFile(dpiFile)(dpi)
    }

    // The Verilator output directory
    val objPath = oPath.resolve("obj_dir")

    // Write simulation main
    val mainFile = oPath.resolve("main.cc").toFile
    writeFile(mainFile) {
      val rstActive = resetStyle match {
        case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => 1
        case _                                          => 0
      }
      val rstInactive = 1 - rstActive
      val rstSync = resetStyle match {
        case ResetStyle.SyncHigh | ResetStyle.SyncLow => 1
        case _                                        => 0
      }

      s"""#include <assert.h>
         |#include <inttypes.h>
         |#include <stdlib.h>
         |
         |#include <verilated.h>
         |#include "Vtestbench.h"
         |
         |#if VM_TRACE
         |# include <verilated_vcd_c.h>
         |#endif
         |
         |#define HALF_CLK_PERIOD 500 // 1ns clock
         |
         |static uint64_t main_time = 0;
         |
         |double sc_time_stamp() {
         |  return main_time;
         |}
         |
         |void vl_stop(const char* filename, int linenum, const char* hier) VL_MT_UNSAFE {
         |  (void)filename;
         |  (void)linenum;
         |  (void)hier;
         |  Verilated::gotFinish(true);
         |  Verilated::flushCall();
         |  throw 2;
         |}
         |
         |int main(int argc, char **argv) {
         |  assert(argc == 1);
         |  (void)argv;
         |
         |  Vtestbench testbench;
         |
         |#if VM_TRACE
         |  Verilated::traceEverOn(true);
         |  VerilatedVcdC trace;
         |  trace.set_time_resolution("1ps");
         |  testbench.trace(&trace, 99);
         |  trace.open("trace.vcd");
         |# define EVAL() do { testbench.eval(); trace.dump(main_time); } while(false)
         |#else
         |# define EVAL() testbench.eval();
         |#endif
         |
         |  // Initialize
         |  testbench.$reset = $rstInactive;
         |  testbench.$clock = 0;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Assert reset asynchronously
         |  testbench.$reset = $rstActive;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Apply a clock pulse when using sync reset
         |  testbench.$clock = $rstSync;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // De-assert reset
         |  testbench.$reset = $rstInactive;
         |  testbench.$clock = 0;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Tick the clock
         |  int exit_code = 0;
         |  while (!Verilated::gotFinish()) {
         |    testbench.$clock = !testbench.$clock;
         |    try {
         |      EVAL();
         |      main_time += HALF_CLK_PERIOD;
         |    } catch (int code) {
         |      exit_code = code;
         |      break;
         |    }
         |    if (main_time > ${timeout * 1000}L) {
         |      printf("TIMEOUT at %luns\\n", (long)(main_time/1000));
         |      exit_code = 1;
         |      break;
         |    }
         |  }
         |
         |  // Finished simulation
         |  testbench.final();
         |
         |#if VM_TRACE
         |  trace.close();
         |#endif
         |
         |  exit(exit_code);
         |}
         |""".stripMargin
    }

    // Run verilator
    val verilatorCmd = {
      val traceOpts = if (trace) "--trace" else ""
      val cflags = "-DVL_USER_STOP"
      val files = s"$tbFile $mainFile ${dpiFileOpt.fold("")(_.toString)}"
      s"""$verilator --cc --exe -Wall $traceOpts -CFLAGS "$cflags" -O3 --assert --trace-underscore -y $oPath --Mdir $objPath $files"""
    }
    system(verilatorCmd, oPath, "verilator-compile.log")

    // Build model
    system(s"make -C $objPath -f Vtestbench.mk", oPath, "verilator-build.log")

    // Run model and check output
    val lines = Process(s"${objPath.resolve("Vtestbench")}", oPath.toFile).lazyLines_!
    val patterns = expect.split("\n") map { _.trim }
    if (!allMatches(patterns, lines)) {
      println("Output:")
      lines foreach println
      println("Expected:")
      patterns foreach println
      fail("Simulation output does not match expected")
    }
  }

  // Structural equivalence checking with yosys
  private def yosysEquiv(
      topLevel: String,
      goldenFile: File,
      tmpDir: Path
    )(
      implicit
      config: Config
    ): Boolean = {
    val scriptFile = tmpDir.resolve("equiv.ys").toFile
    writeFile(scriptFile) {
      s"""read_verilog $goldenFile
         |prep -flatten -top $topLevel
         |memory
         |opt -full $topLevel
         |design -stash gold
         |
         |read_verilog ${tmpDir.resolve(topLevel + ".v").toFile}
         |hierarchy -check -libdir $tmpDir -top $topLevel
         |prep -flatten -run coarse: -top $topLevel
         |memory
         |opt -full $topLevel
         |design -stash comp
         |
         |design -copy-from gold -as gold $topLevel
         |design -copy-from comp -as comp $topLevel
         |
         |equiv_make gold comp equiv
         |prep -flatten -top equiv
         |opt -full equiv
         |opt_clean -purge
         |
         |#show -prefix equiv-prep -colors 1 -stretch
         |
         |equiv_simple -seq 5
         |equiv_induct -seq 20
         |
         |equiv_status -assert
         |""".stripMargin
    }

    system(
      s"$yosys -s $scriptFile ${if (config.verbose < 2) "-q" else ""}",
      tmpDir,
      "yosys-equiv.log",
      failOk = true
    )
  }

  // BMC/k-induction based equivalence checking using SymbiYosys
  private def symbiYosysEquiv(
      topLevel: String,
      goldenFile: File,
      oPath: Path,
      manifest: Json,
      fec: Map[String, String]
    )(
      implicit
      config: Config
    ): Boolean = {
    // Generate the miter circuit
    val topLevelManifest = manifest.hcursor.downField("top-levels").downField(topLevel)

    val clock = topLevelManifest.get[String]("clock").toOption
    val reset = topLevelManifest.get[String]("reset").toOption

    val resetStyle = topLevelManifest.get[String]("reset-style") match {
      case Right("sync-high")  => ResetStyle.SyncHigh
      case Right("sync-low")   => ResetStyle.SyncLow
      case Right("async-high") => ResetStyle.AsyncHigh
      case Right("async-low")  => ResetStyle.AsyncLow
      case other               => fail("Unknown reset style: " + other)
    }

    case class Port(dir: String, fc: String)
    case class Signal(
        port: String,
        component: String,
        width: Option[Int],
        signed: Option[Boolean],
        offset: Option[Int])

    implicit val portDecoder: io.circe.Decoder[Port] =
      io.circe.Decoder.forProduct2("dir", "flow-control")(Port.apply)
    implicit val signalDecoder: io.circe.Decoder[Signal] = io.circe.generic.semiauto.deriveDecoder

    val ports = topLevelManifest.downField("ports").as[ListMap[String, Port]].getOrElse(fail)
    val signals = topLevelManifest.downField("signals").as[ListMap[String, Signal]].getOrElse(fail)

    // All Verilog inputs
    val vInputs: Iterable[String] = signals collect {
      case (name, Signal(port, "payload", _, _, _)) if ports(port).dir == "in" => name
      case (name, Signal(port, "valid", _, _, _)) if ports(port).dir == "in"   => name
      case (name, Signal(port, "ready", _, _, _)) if ports(port).dir == "out"  => name
    }

    // All verilog outputs
    val vOutputs: Iterable[String] = signals collect {
      case (name, Signal(port, "payload", _, _, _)) if ports(port).dir == "out" => name
      case (name, Signal(port, "valid", _, _, _)) if ports(port).dir == "out"   => name
      case (name, Signal(port, "ready", _, _, _)) if ports(port).dir == "in"    => name
    }

    // Map from payload signal to corresponding valid
    val vValidOfPayload: Map[String, String] = signals flatMap {
      case (name, Signal(port, "payload", _, _, _)) =>
        signals collectFirst { case (valid, Signal(`port`, "valid", _, _, _)) => name -> valid }
      case _ => None
    }

    val widthOf: Map[String, Int] = signals map {
      case (name, Signal(_, _, Some(width), _, _)) => name -> width
      case (name, _)                               => name -> 1
    }

    val miterFile = oPath.resolve("__miter.v").toFile
    val mpw = new PrintWriter(miterFile)

    def writeDecl(
        prefix: String,
        width: Int,
        name: String,
        end: String
      ): Unit = width match {
      case 1 => mpw.write(s"  $prefix $name$end\n")
      case w => mpw.write(s"  $prefix [${w - 1}:0] $name$end\n")
    }

    // Module header
    mpw.write("module miter(\n")
    if (clock.isEmpty) {
      mpw.write("  input wire clk")
      if (vInputs.nonEmpty) mpw.write(",")
      mpw.write("\n")
    }
    (vInputs.iterator filterNot { reset contains _ }).zipWithIndex foreach {
      case (name, idx) =>
        val end = if (idx < vInputs.size - 1) "," else ""
        writeDecl("input wire", widthOf(name), name, end)
    }
    mpw.write(");\n")

    // Reset logic
    reset foreach { name =>
      mpw.write("  // Reset logic\n")
      // Note: This isn't quite semantically right for async resets, as there
      // is no active reset edge, but will do for now for equivalence checking
      resetStyle match {
        case ResetStyle.SyncHigh | ResetStyle.AsyncHigh =>
          mpw.write {
            s"""  reg $name = 1;
               |  always @(posedge clk) $name <= 1'd0;
               |""".stripMargin
          }
        case ResetStyle.SyncLow | ResetStyle.AsyncLow =>
          mpw.write {
            s"""  reg $name = 0;
               |  always @(posedge clk) $name <= 1'd1;
               |""".stripMargin
          }
      }
    }

    // Output nets
    mpw.write("\n")
    mpw.write("  // Outputs of golden\n")
    vOutputs foreach { name =>
      writeDecl("wire", widthOf(name), s"golden__$name", ";")
    }
    mpw.write("\n")
    mpw.write("  // Outputs of alogic\n")
    vOutputs foreach { name =>
      writeDecl("wire", widthOf(name), s"alogic__$name", ";")
    }

    mpw.write("\n")
    mpw.write("  // Golden instance\n")
    mpw.write("  golden golden (\n")
    vInputs foreach { name => mpw.write(s"    .$name($name),\n") }
    vOutputs.iterator.zipWithIndex foreach {
      case (name, idx) =>
        val end = if (idx < vOutputs.size - 1) ",\n" else "\n"
        mpw.write(s"    .$name(golden__$name)$end")
    }
    mpw.write("  );\n")
    mpw.write("\n")
    mpw.write("  // Alogic instance\n")
    mpw.write("  alogic alogic (\n")
    vInputs foreach { name => mpw.write(s"    .$name($name),\n") }
    vOutputs.iterator.zipWithIndex foreach {
      case (name, idx) =>
        val end = if (idx < vOutputs.size - 1) ",\n" else "\n"
        mpw.write(s"    .$name(alogic__$name)$end")
    }
    mpw.write("  );\n")

    mpw.write("\n")
    mpw.write("  // Output comparators\n")
    vOutputs foreach { name =>
      val assign = vValidOfPayload.get(name) match {
        case Some(valid) => s" = (golden__$name === alogic__$name) || ~alogic__$valid;"
        case None        => s" = (golden__$name === alogic__$name);"
      }
      writeDecl("wire", 1, s"ok__$name", assign)
    }

    mpw.write("\n")
    mpw.write("  // The equivalence signal\n")
    mpw.write(
      vOutputs.iterator
        .map(n => s"ok__$n")
        .mkString("  wire equiv = &{\n    ", ",\n    ", "\n  };\n")
    )

    mpw.write("\n")
    mpw.write("  // assertions\n")
    reset match {
      case None => mpw.write("  always @(posedge clk) assert(equiv);\n");
      case Some(rst) =>
        resetStyle match {
          case ResetStyle.SyncHigh | ResetStyle.AsyncHigh =>
            mpw.write(s"  always @(posedge clk) if (!$rst) assert(equiv);\n");
          case ResetStyle.SyncLow | ResetStyle.AsyncLow =>
            mpw.write(s"  always @(posedge clk) if ($rst) assert(equiv);\n");
        }
    }

    mpw.write("\nendmodule\n")
    mpw.flush()
    mpw.close()

    // Write constraints file
    val constraintsFileOpt = fec.get("smtc") map { constraints =>
      val file = oPath.resolve("__constraints.smtc").toFile
      val pw = new PrintWriter(file)
      pw.write(constraints)
      pw.flush()
      pw.close()
      file
    }

    // Write sby script
    val scriptFile = oPath.resolve("equiv.sby").toFile
    writeFile(scriptFile) {
      s"""[options]
         |mode ${fec.getOrElse("mode", if (clock.isDefined) "prove" else "bmc")}
         |depth ${fec.getOrElse("depth", if (clock.isDefined) 20 else 2)}
         |timeout ${fec.getOrElse("timeout", "10")}
         |${constraintsFileOpt map { "smtc " + _.getName } getOrElse "# No constraints"}
         |
         |[engines]
         |smtbmc ${fec.getOrElse("solver", "z3")}
         |
         |[script]
         |# Read the reference implementation
         |read_verilog -formal ${goldenFile.getName}
         |rename $topLevel golden
         |
         |# Read the compiled design
         |read_verilog ${oPath.resolve(topLevel + ".v").toFile.getName}
         |rename $topLevel alogic
         |
         |# Read the miter circuit
         |read_verilog -formal ${miterFile.getName}
         |
         |# Resolve and prep
         |hierarchy -check -libdir $oPath -top miter
         |prep -run coarse: -top miter
         |
         |#memory; flatten; opt -full
         |#show -colors 1 -stretch miter
         |
         |[files]
         |${topLevel + ".v"}
         |${goldenFile.getName}
         |${miterFile.getName}
         |${constraintsFileOpt.map(_.getName).getOrElse("# No constraints file")}
         |""".stripMargin
    }

    // Perform the equivalence check
    val sbyCmd = s"$symbiyosys ${scriptFile.getName}"
    val path = s"$symbiyosysPath:${scala.util.Properties.envOrElse("PATH", "")}"
    system(sbyCmd, oPath, "sby-equiv.log", failOk = true, Map("PATH" -> path))
  }

  private def formalEquivalenceCheck(
      topLevel: String,
      fec: Map[String, String],
      oDir: Path,
      manifeset: Json
    )(
      implicit
      config: Config
    ): Unit = {
    // Write the golden model
    val goldenFile = oDir.resolve("__golden.v").toFile
    writeFile(goldenFile)(fec("golden"))

    // Lint the golden model, just to be sure, as yosys "helpfully" provides
    // implicit declarations of undeclared names.
    system(
      s"$verilator --lint-only -Wall -Wno-DECLFILENAME -Wno-UNUSED ${goldenFile.getName}",
      oDir,
      "verilator-golden-lint.log"
    )

    // Now strip `line because it's too much for yosys...
    system(s"sed -i s/`line.*// ${goldenFile.getName}", oDir, "sed.log")

    // First check the simple way if that fails try the hard way
    lazy val easyOK = yosysEquiv(topLevel, goldenFile, oDir)
    lazy val hardOK = symbiYosysEquiv(topLevel, goldenFile, oDir, manifeset, fec)

    if (!easyOK && !hardOK) {
      fail("FEC failed")
    }
  }

  private def checkJson(name: String, json: Json, expected: Map[String, String]): Unit = {
    expected foreach {
      case (keys, value) =>
        val root: io.circe.ACursor = json.hcursor
        val selection = keys.split("/").foldLeft(root)({ case (c, k) => c.downField(k) })
        val expectedValue = io.circe.parser.parse(value) match {
          case Left(failure) =>
            fail(s"Failed to parse expected '$name' entry '$keys': " + failure.message)
          case Right(json) => json
        }
        selection.focus match {
          case None       => fail(s"No entry '$keys' in '$name'")
          case Some(json) => assert(json == expectedValue, s"'$name' - '$keys'")
        }
    }
  }

  sealed private trait MessageSpec {
    val fileLineOpt: Option[(String, Int)]
    val patterns: List[String]

    def matches(message: Message): Boolean = {
      val typeMatches = (this, message) match {
        case (_: WarningSpec, _: Warning) => true
        case (_: ErrorSpec, _: Error)     => true
        case (_: NoteSpec, _: Note)       => true
        case (_: FatalSpec, _: Fatal)     => true
        case _                            => false
      }
      lazy val locMatches = (message.loc, fileLineOpt) match {
        case (Loc.unknown, None) => true // No location in Message, nor MessageSpec
        case (loc, Some((file, line))) =>
          loc.line == line && // Right line number
            file.r.pattern.matcher(loc.file).matches() // Right file pattern
        case _ => false
      }
      typeMatches && // Right type
      locMatches && // Right location
      allMatches(patterns, message.msg) // Right text
    }

    def render: String = {
      val kindString = this match {
        case _: WarningSpec => "WARNING"
        case _: ErrorSpec   => "ERROR"
        case _: NoteSpec    => "NOTE"
        case _: FatalSpec   => "FATAL"
      }
      val prefix = fileLineOpt match {
        case Some((file, line)) => s"$file:$line: $kindString: "
        case None               => s"$kindString: "
      }
      val triplets =
        LazyList.continually(prefix) lazyZip ("" +: LazyList.continually("... ")) lazyZip patterns
      triplets map { case (a, b, c) => a + b + c } mkString "\n"
    }

  }

  // format: off
  private case class WarningSpec(fileLineOpt: Option[(String, Int)], patterns: List[String]) extends MessageSpec
  private case class ErrorSpec(fileLineOpt: Option[(String, Int)], patterns: List[String]) extends MessageSpec
  private case class NoteSpec(fileLineOpt: Option[(String, Int)], patterns: List[String]) extends MessageSpec
  private case class FatalSpec(fileLineOpt: Option[(String, Int)], patterns: List[String]) extends MessageSpec
  // format: on

  private def parseCheckFile(checkFile: File): (
      Map[String, String],
      Map[String, Map[String, String]],
      List[MessageSpec]
  ) = {
    val attrs = mutable.Map[String, String]()
    val dicts = mutable.Map[String, Map[String, String]]()

    val pairMatcher = """@([^:]+):(.*)""".r
    val longMatcher = """@(.+)\{\{\{""".r
    val boolMatcher = """@(.+)""".r
    val mesgMatcher = """((.*):(\d+): )?(WARNING|ERROR|NOTE|FATAL): (\.\.\. )?(.*)""".r

    val mesgSpecs = new ListBuffer[MessageSpec]

    val mesgBuff = new ListBuffer[String]
    var mesgType: String = ""
    var mesgFileLineOpt: Option[(String, String)] = None

    def finishPendingMessageSpec(): Unit = if (mesgBuff.nonEmpty) {
      val fileLineOpt = mesgFileLineOpt match {
        case Some(("", string)) =>
          Some((".*" + checkFile.getAbsolutePath.split("/").last, string.toInt))
        case Some((pattern, string)) => Some((pattern, string.toInt))
        case None                    => None
      }
      mesgType match {
        case "WARNING" => mesgSpecs append WarningSpec(fileLineOpt, mesgBuff.toList)
        case "ERROR"   => mesgSpecs append ErrorSpec(fileLineOpt, mesgBuff.toList)
        case "NOTE"    => mesgSpecs append NoteSpec(fileLineOpt, mesgBuff.toList)
        case "FATAL"   => mesgSpecs append FatalSpec(fileLineOpt, mesgBuff.toList)
        case _         => unreachable
      }
      mesgBuff.clear()
    }

    var key = ""
    val buf = new ListBuffer[String]

    def add(key: String, value: String): Unit = {
      val (a, b) = key.span(_ != '/')
      if (b.isEmpty) {
        attrs(a.trim) = value
      } else {
        val pair = b.tail.trim -> value
        dicts.updateWith(a.trim) {
          case Some(m) => Some(m + pair)
          case None    => Some(Map(pair))
        }
      }
    }

    val source = Source(checkFile)

    for {
      (line, lineNo) <- source.linesIterator zip LazyList.from(1)
      if line startsWith "//"
    } {
      val text = line.drop(2)
      text.trim match {
        case pairMatcher(k, v) if key == "" => add(k, v.trim)
        case longMatcher(k) if key == "" =>
          key = k
          key filterNot { _.isWhitespace } match {
            case "fec/golden" => buf append s"`line ${lineNo + 1} ${checkFile.getAbsolutePath} 0"
            case _            =>
          }
        case "}}}" if key != ""          => add(key, buf mkString "\n"); buf.clear(); key = ""
        case _ if key != ""              => buf append text
        case boolMatcher(k) if key == "" => add(k, "")
        case mesgMatcher(fileLine, file, line, kind, null, pattern) if key == "" =>
          finishPendingMessageSpec()
          mesgFileLineOpt = if (fileLine != null) Some((file.trim, line.trim)) else None
          mesgType = kind.trim
          mesgBuff append pattern
        case mesgMatcher(fileLine, file, line, kind, _, pattern) if key == "" =>
          val mflo = mesgFileLineOpt
          val fileOk = (fileLine == null && mflo.isEmpty) || (mflo.get._1 == file.trim)
          val lineOk = (fileLine == null && mflo.isEmpty) || (mflo.get._2 == line.trim)
          val typeOk = mesgType == kind.trim
          assert(fileOk, "Message continuation must have same file pattern")
          assert(lineOk, "Message continuation must have same line number")
          assert(typeOk, "Message continuation must have same message type")
          mesgBuff append pattern
        case _ =>
      }
    }

    if (key.nonEmpty) { fail(s"Missing closing }}} for '${key.trim}' in test file") }

    finishPendingMessageSpec()

    (attrs.toMap, dicts.toMap, mesgSpecs.toList)
  } tap {
    case (attr, dict, _) =>
      val validAttr = Set(
        "args",
        "expect-file",
        "ignore",
        "out-top",
        "source-file",
        "verilator-lint-off"
      )
      attr.keysIterator foreach { k =>
        if (!validAttr(k)) { fail(s"Unknown test attribute '$k'") }
      }
      val validDict = Set(
        "fec",
        "manifest",
        "sim",
        "stats"
      )
      dict.keysIterator foreach { k =>
        if (!validDict(k)) { fail(s"Unknown test dictionary attribute '$k'") }
      }
  }

  private object EndToEndTest extends Tag("com.argondesign.alogic.tags.EndToEndTest")

  def defineTest(testName: String, sourceFile: File): Unit =
    testName taggedAs EndToEndTest in { configMap: ConfigMap =>
      // Parse the check file
      val (attr, dict, messageSpecs) = parseCheckFile(sourceFile)

      // Cancel test if required
      if (attr contains "ignore") {
        cancel("@ignore")
      }

      // Create config
      implicit val config: Config = Config(
        trace = configMap.getWithDefault("trace", "0").toInt != 0,
        verbose = configMap.getWithDefault("verbose", "0").toInt
      )

      // Create output directory and runt he test
      withTmpDir(configMap.getOptional[String]("tmpdir")) { oPath =>
        // Create argument array
        val args = {
          val buf = new mutable.ListBuffer[String]

          // Arguments always required
          buf append "-o"
          buf append oPath.toAbsolutePath.toString

          val sourceDir = sourceFile.toPath.getParent

          def addArgs(args: String): Unit =
            args
              .split(" ")
              .filter(_.nonEmpty)
              .map(_.replaceAll("\\$TESTDIR", sourceDir.toString))
              .foreach(buf.append)

          // Arguments specified in the tests
          attr.get("args") foreach addArgs

          // Arguments specified to ScalaTest
          configMap.getOptional[String]("args") foreach addArgs

          // Override default reset style to make FEC easier
          if (!(buf contains "--reset-style")) {
            buf append "--reset-style"
            buf append "sync-high"
          }

          // Generate stats is we are checking them
          if (dict contains "stats") {
            buf append "--stats"
          }

          // The input source file
          buf append attr.getOrElse("source-file", sourceFile.getAbsolutePath)

          buf.toSeq
        }

        if (config.verbose >= 2) {
          println(s"${AnsiColor.BOLD}=== command line arguments:${AnsiColor.RESET}")
          args foreach println
        }

        // Do the compilation
        val runReturn =
          try {
            CommandLineInterface.run(args)
          } catch {
            // For some reason ScalaTest swallows stack overflow exceptions,
            // wrapping them makes them show up in the test runner.
            case e: StackOverflowError =>
              throw new RuntimeException(e)
          }

        val messages = {
          val messageBuffer: MessageBuffer = runReturn._1
          messageBuffer.messages
        }

        // Cancel here if coverageEnabled as we have all the coverage we want,
        // and this speeds up running the test suite up considerably.
        if (BuildInfo.coverageEnabled) {
          messages foreach { _.render } // Render messages still to fill coverage
          cancel("Cancel CompilationTest checks because coverageEnabled")
        }

        // Check messages
        {
          // Each message spec should consume one message, in the given order
          val mIt = messages.iterator map { Some(_) }
          val sIt = messageSpecs.iterator map { Some(_) }
          mIt.zipAll(sIt, None, None) filterNot {
            case (Some(message), Some(spec)) => spec matches message
            case _                           => false
          } pipe {
            case iterator if iterator.isEmpty => // All OK
            case iterator =>
              iterator foreach {
                case (Some(message), Some(spec)) =>
                  println("Mismatched message, expected:")
                  println(spec.render)
                  println("actual:")
                  println(message.render)
                case (Some(message), None) =>
                  println("Unexpected message:")
                  println(message.render)
                case (None, Some(spec)) =>
                  println("Missing message:")
                  println(spec.render)
                case _ => unreachable
              }
              fail("Message check failed")
          }
        }

        // If an ERROR or a FATAL is expected, the compilation should have failed
        val expectedToFail = messageSpecs exists {
          case _: ErrorSpec => true
          case _: FatalSpec => true
          case _            => false
        }

        // Check compilation status
        val retCode: Int = runReturn._3
        (retCode != 0) shouldBe expectedToFail

        if (!expectedToFail) {
          //////////////////////////////////////////////////////////////////////
          // Check expected output file exists
          //////////////////////////////////////////////////////////////////////

          attr
            .get("expect-file")
            .iterator
            .flatMap(_ split "\n")
            .map(_.trim)
            .foreach { name =>
              if (!(oPath resolve name).toFile.exists) {
                fail(s"Expected output file was not created '$name'")
              }
            }

          //////////////////////////////////////////////////////////////////////
          // Load and check manifest
          //////////////////////////////////////////////////////////////////////

          val manifest =
            io.circe.parser.parse(Source((oPath resolve "manifest.json").toFile).text) match {
              case Left(failure) => fail("Failed to parse manifest: " + failure.message)
              case Right(json)   => json
            }

          dict get "manifest" foreach { expected =>
            checkJson("manifest.json", manifest, expected)
          }

          //////////////////////////////////////////////////////////////////////
          // Load and check stats
          //////////////////////////////////////////////////////////////////////

          dict get "stats" foreach { expected =>
            val stats =
              io.circe.parser.parse(Source((oPath resolve "stats.json").toFile).text) match {
                case Left(failure) => fail("Failed to parse stats: " + failure.message)
                case Right(json)   => json
              }

            checkJson("stats.json", stats, expected)
          }

          val outTop = attr.getOrElse(
            "out-top",
            manifest.hcursor.downField("top-levels").keys.get.toSeq match {
              case Seq(t) => t
              case _      => fail("Need @out-top")
            }
          )

          dict get "sim" match {
            // Build and run testbench if provided
            case Some(sim) =>
              val test = sim("test")
              val expect = sim.getOrElse("expect", "")
              val dpi = sim.getOrElse("dpi", "")
              val timeout = sim.getOrElse("timeout", "100").toLong
              val trace = config.trace
              verilatorSim(outTop, test, expect, dpi, timeout, trace, oPath, manifest)
            case None =>
              // Lint unless told not to
              if (!(attr contains "verilator-lint-off")) {
                verilatorLint(outTop, oPath)
              }
          }

          dict get "fec" foreach { fec =>
            // Perform equivalence check with golden reference
            formalEquivalenceCheck(outTop, fec, oPath, manifest)
          }
        }
      }
    }

}
