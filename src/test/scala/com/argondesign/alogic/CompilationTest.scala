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
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Fatal
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable
import io.circe.Json
import org.scalatest.ConfigMap
import org.scalatest.ParallelTestExecution
import org.scalatest.fixture
import org.scalatest.freespec.FixtureAnyFreeSpec

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

  def getTool(path: Path, name: String, msg: String): File = {
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
  case class Config(
      trace: Boolean,
      verbose: Int)

  // Create temporary directory, run function passing the path to the temporary
  // directory as argument, then remove the temporary directory
  def withTmpDir[R](tmpDir: Option[String])(f: Path => Unit): Unit = {
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

  def writeFile(file: File)(content: String): Unit = {
    val pw = new PrintWriter(file)
    pw.write(content)
    pw.flush()
    pw.close()
  }

  def system(
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

  def allMatches(patterns: Iterable[String], lines: Iterable[String]): Boolean = {
    lines.sizeIs == patterns.size && { // Right length
      (patterns.iterator zip lines.iterator) forall { // Right content
        case (pattern, line) => pattern.r.pattern.matcher(line).matches()
      }
    }
  }

  // Lint compiler output with Verilator
  def verilatorLint(topLevel: String, oPath: Path)(implicit config: Config): Unit = {
    // Lint the top level
    system(s"$verilator --lint-only -Wall -y $oPath $topLevel", oPath, "verilator-lint.log")
  }

  // Build and run simulation with Verilator
  def verilatorSim(
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
  def yosysEquiv(
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
  def symbiYosysEquiv(
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

    val vInputs = signals collect {
      case (name, Signal(port, "payload", _, _, _)) if ports(port).dir == "in" => name
      case (name, Signal(port, "valid", _, _, _)) if ports(port).dir == "in"   => name
      case (name, Signal(port, "ready", _, _, _)) if ports(port).dir == "out"  => name
    }

    val vOutputs = signals collect {
      case (name, Signal(port, "payload", _, _, _)) if ports(port).dir == "out" => name
      case (name, Signal(port, "valid", _, _, _)) if ports(port).dir == "out"   => name
      case (name, Signal(port, "ready", _, _, _)) if ports(port).dir == "in"    => name
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
      writeDecl("wire", 1, s"cmp__$name", s" = golden__$name === alogic__$name;")
    }

    mpw.write("\n")
    mpw.write("  // The equivalence signal\n")
    mpw.write(
      vOutputs.iterator
        .map(n => s"cmp__$n")
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

  def formalEquivalenceCheck(
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

    // First check the simple  if that fails try the hard way
    lazy val easyOK = yosysEquiv(topLevel, goldenFile, oDir)
    lazy val hardOK = symbiYosysEquiv(topLevel, goldenFile, oDir, manifeset, fec)

    if (!easyOK && !hardOK) {
      fail("FEC failed")
    }
  }

  sealed private trait MessageSpec {
    val fileLineOpt: Option[(String, Int)]
    val patterns: List[String]

    def matches(message: Message): Boolean = {
      val typeMatches = (this, message) match {
        case (_: WarningSpec, _: Warning) => true
        case (_: ErrorSpec, _: Error)     => true
        case (_: FatalSpec, _: Fatal)     => true
        case _                            => false
      }
      lazy val locMatches = (message.locOpt, fileLineOpt) match {
        case (Some(loc), Some((file, line))) =>
          loc.line == line && // Right line number
            file.r.pattern.matcher(loc.file).matches() // Right file pattern
        case (None, None) => true // No location in Message, nor MessageSpec
        case _            => false
      }
      typeMatches && // Right type
      locMatches && // Right location
      allMatches(patterns, message.msg) // Right text
    }

    def render: String = {
      val kindString = this match {
        case _: WarningSpec => "WARNING"
        case _: ErrorSpec   => "ERROR"
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
  private case class FatalSpec(fileLineOpt: Option[(String, Int)], patterns: List[String]) extends MessageSpec
  // format: on

  private def parseCheckFile(checkFile: String): (
      Map[String, String],
      Map[String, Map[String, String]],
      List[MessageSpec]
  ) = {
    val attrs = mutable.Map[String, String]()
    val dicts = mutable.Map[String, Map[String, String]]()

    val pairMatcher = """@([^:]+):(.*)""".r
    val longMatcher = """@(.+)\{\{\{""".r
    val boolMatcher = """@(.+)""".r
    val mesgMatcher = """((.*):(\d+): )?(WARNING|ERROR|FATAL): (\.\.\. )?(.*)""".r

    val mesgSpecs = new ListBuffer[MessageSpec]

    val mesgBuff = new ListBuffer[String]
    var mesgType: String = ""
    var mesgFileLineOpt: Option[(String, String)] = None

    def finishPendingMessageSpec(): Unit = if (mesgBuff.nonEmpty) {
      val fileLineOpt = mesgFileLineOpt match {
        case Some(("", string))      => Some((".*" + checkFile.split("/").last, string.toInt))
        case Some((pattern, string)) => Some((pattern, string.toInt))
        case None                    => None
      }
      mesgType match {
        case "WARNING" => mesgSpecs append WarningSpec(fileLineOpt, mesgBuff.toList)
        case "ERROR"   => mesgSpecs append ErrorSpec(fileLineOpt, mesgBuff.toList)
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
            case "fec/golden" => buf append s"`line ${lineNo + 1} $checkFile 0"
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

    finishPendingMessageSpec()

    (attrs.toMap, dicts.toMap, mesgSpecs.toList)
  } tap {
    case (attr, dict, _) =>
      val validAttr = Set(
        "args",
        "expect-file",
        "ignore",
        "out-top",
        "top",
        "verilator-lint-off"
      )
      attr.keysIterator foreach { k =>
        if (!validAttr(k)) { fail(s"Unknown test attribute '$k'") }
      }
      val validDict = Set(
        "fec",
        "manifest",
        "sim"
      )
      dict.keysIterator foreach { k =>
        if (!validDict(k)) { fail(s"Unknown test dictionary attribute '$k'") }
      }
  }

  def defineTest(
      name: String,
      searchPath: Path,
      top: String,
      checkFile: String
    ): Unit = {
    name in { configMap: ConfigMap =>
      // Parse the check file
      val (attr, dict, messageSpecs) = parseCheckFile(checkFile)

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
          val buf = new mutable.ArrayBuffer[String]

          // Arguments always required
          buf append "-y"
          buf append searchPath.toAbsolutePath.toString
          buf append "-o"
          buf append oPath.toAbsolutePath.toString

          // Arguments specified in the tests
          attr.get("args") foreach {
            _.split(" ") filter { _.nonEmpty } foreach buf.append
          }

          // Arguments specified to ScalaTest
          configMap.getOptional[String]("args") foreach {
            _.split(" ") filter { _.nonEmpty } foreach buf.append
          }

          // Override default reset style to make FEC easier
          if (!(buf contains "--reset-style")) {
            buf append "--reset-style"
            buf append "sync-high"
          }

          // The top-level specifier
          buf append attr.getOrElse("top", top)

          buf.toArray
        }

        if (config.verbose >= 2) {
          println(s"${AnsiColor.BOLD}=== command line arguments:${AnsiColor.RESET}")
          args foreach println
        }

        // Do the compilation
        val runReturn =
          try {
            Main.run(args)
          } catch {
            // For some reason ScalaTest swallows stack overflow exceptions,
            // wrapping them makes them show up in the test runner.
            case e: StackOverflowError =>
              throw new RuntimeException(e)
          }

        // Check messages
        {
          // fail flag
          var messageCheckFailed = false

          val messages = {
            val messageBuffer: MessageBuffer = runReturn._1
            messageBuffer.messages
          }

          // Group messages by specs
          val messageGroups = messages groupBy { message =>
            messageSpecs find { _ matches message }
          }

          // Fail if a spec matches multiple messages
          for ((Some(spec), messages) <- messageGroups if messages.lengthIs > 1) {
            println("Message pattern:")
            println(spec.render)
            println("Matches multiple messages:")
            messages foreach { message => println(message.render) }
            messageCheckFailed = true
          }

          // Print unexpected messages
          messageGroups.get(None) foreach { unexpectedMessages =>
            unexpectedMessages foreach { message =>
              println("Unexpected message:")
              println(message.render)
            }
            messageCheckFailed = true
          }

          // Print unused patterns
          for {
            spec <- messageSpecs
            if !(messageGroups contains Some(spec))
          } {
            println("Unused message pattern:")
            println(spec.render)
            messageCheckFailed = true
          }

          // Fail test if message check failed
          if (messageCheckFailed) {
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
          attr get "expect-file" foreach { name =>
            // Check expected output file exists
            if (!(oPath resolve name).toFile.exists) {
              fail(s"Expected output file was not created '$name'")
            }
          }

          val manifest = io.circe.parser.parse(Source(oPath resolve "manifest.json").text) match {
            case Left(failure) => fail("Failed to parse manifest: " + failure.message)
            case Right(json)   => json
          }

          dict get "manifest" foreach { expected =>
            expected foreach {
              case (keys, value) =>
                val root: io.circe.ACursor = manifest.hcursor
                val selection = keys.split("/").foldLeft(root)({ case (c, k) => c.downField(k) })
                val expectedValue = io.circe.parser.parse(value) match {
                  case Left(failure) =>
                    fail(s"Failed to parse expected manifest entry '$keys': " + failure.message)
                  case Right(json) => json
                }
                selection.focus match {
                  case None       => fail(s"No entry '$keys' in manifest")
                  case Some(json) => json shouldBe expectedValue
                }
            }

          }

          val outTop = attr.getOrElse("out-top", top)

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

}
