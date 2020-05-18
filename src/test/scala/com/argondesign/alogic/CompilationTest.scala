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
import java.nio.file.Paths

import com.argondesign.alogic.ast.Trees.Decl
import com.argondesign.alogic.ast.Trees.Root
import com.argondesign.alogic.ast.Trees.Tree
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Error
import com.argondesign.alogic.core.Fatal
import com.argondesign.alogic.core.InternalCompilerErrorException
import com.argondesign.alogic.core.Message
import com.argondesign.alogic.core.Settings
import com.argondesign.alogic.core.Warning
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.util.unreachable
import org.scalatest.ConfigMap
import org.scalatest.ParallelTestExecution
import org.scalatest.fixture
import org.scalatest.freespec.FixtureAnyFreeSpec

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.AnsiColor
import scala.io.Source
import scala.sys.process._

trait CompilationTest
    extends FixtureAnyFreeSpec
    with AlogicTest
    with fixture.ConfigMapFixture
    with ParallelTestExecution {

  def getTool(path: Path, name: String, msg: String): File = {
    path.resolve(name).toFile
  } pipe { _.getCanonicalFile } ensuring { _.exists }

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
      dumpTrees: Boolean,
      tmpDir: Option[String],
      trace: Boolean,
      traceElaborate: Boolean,
      verbose: Int)

  // Create temporary directory, run function passing the path to the temporary
  // directory as argument, then remove the temporary directory
  def withTmpDir[R](f: Path => R)(implicit config: Config): R = {
    def del(f: File): Unit = {
      if (f.isDirectory) {
        f.listFiles foreach del
      }
      f.delete()
    }
    config.tmpDir match {
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

  // Will be updated form multiple threads so must be thread safe
  private val outputs = scala.collection.concurrent.TrieMap[String, String]()

  // Insert compiler output to the 'outputs' map above
  private def outputWriterFactory(
      treeAndSuffixOrFileName: Either[(Tree, String), String]
    ): Writer = {
    new StringWriter {
      override def close(): Unit = {
        treeAndSuffixOrFileName match {
          case Left((decl: Decl, suffix)) => outputs(decl.symbol.name + suffix) = this.toString
          case Left((root: Root, suffix)) =>
            outputs(root.loc.source.file.getName.split('.').head + suffix) = this.toString
          case Right(fileName) => outputs(fileName) = this.toString
          case _               => ???
        }
        super.close()
      }
    }
  }

  // Compiler message buffer
  private val messages = new ListBuffer[Message]

  // Save messages to the buffer above
  private def messageEmitter(msg: Message, cc: CompilerContext) = {
    messages synchronized {
      messages append msg
    }
  }

  def writeFile(file: File)(content: String): Unit = {
    val pw = new PrintWriter(file)
    pw.write(content)
    pw.flush()
    pw.close()
  }

  def writeOutputs(path: Path): Unit = outputs foreach {
    case (name, text) => writeFile(path.resolve(name).toFile)(text)
  }

  def checkFileExists(name: String): Unit =
    if (!(outputs contains name)) {
      val files = outputs.keys mkString "\n"
      fail(s"Could not find file '$name' among \n$files\n")
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
      val source = Source.fromFile(logFile)
      try {
        source.getLines foreach println
      } finally {
        source.close()
      }
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
  def verilatorLint(topLevel: String, tmpDir: Path)(implicit config: Config): Unit = {
    // Write all files to temporary directory
    writeOutputs(tmpDir)

    // Lint the top level
    system(s"$verilator --lint-only -Wall -y $tmpDir $topLevel", tmpDir, "verilator-lint.log")
  }

  // Build and run simulation with Verilator
  def verilatorSim(
      topLevel: String,
      test: String,
      expect: String,
      dpi: String,
      timeout: Long,
      trace: Boolean,
      tmpDir: Path
    )(
      implicit
      cc: CompilerContext,
      config: Config
    ): Unit = {
    require(topLevel != "testbench")
    assert(outputs forall { _._1 != "testbench" })

    // Write all files to temporary directory
    writeOutputs(tmpDir)

    // Write the testbench
    val tbFile = tmpDir.resolve("testbench.sv").toFile
    writeFile(tbFile) {
      s"""module testbench(
         |  input wire clk,
         |  input wire ${cc.rst}
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
    val dpiFileOpt = Option.when(dpi.nonEmpty)(tmpDir.resolve("dpi.cc").toFile)
    dpiFileOpt foreach { dpiFile =>
      writeFile(dpiFile)(dpi)
    }

    // The Verilator output directory
    val objPath = tmpDir.resolve("obj_dir")

    // Write simulation main
    val mainFile = tmpDir.resolve("main.cc").toFile
    writeFile(mainFile) {
      val rstActive = cc.settings.resetStyle match {
        case ResetStyle.AsyncHigh | ResetStyle.SyncHigh => 1
        case _                                          => 0
      }
      val rstInactive = 1 - rstActive
      val rstSync = cc.settings.resetStyle match {
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
         |  testbench.${cc.rst} = $rstInactive;
         |  testbench.clk = 0;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Assert reset asynchronously
         |  testbench.${cc.rst} = $rstActive;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Apply a clock pulse when using sync reset
         |  testbench.clk = $rstSync;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // De-assert reset
         |  testbench.${cc.rst} = $rstInactive;
         |  testbench.clk = 0;
         |  EVAL();
         |  main_time += HALF_CLK_PERIOD;
         |
         |  // Tick the clock
         |  int exit_code = 0;
         |  while (!Verilated::gotFinish()) {
         |    testbench.clk = !testbench.clk;
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
      s"""$verilator --cc --exe -Wall $traceOpts -CFLAGS "$cflags" -O3 --assert --trace-underscore -y $tmpDir --Mdir $objPath $files"""
    }
    system(verilatorCmd, tmpDir, "verilator-compile.log")

    // Build model
    system(s"make -C $objPath -f Vtestbench.mk", tmpDir, "verilator-build.log")

    // Run model and check output
    val lines = Process(s"${objPath.resolve("Vtestbench")}", tmpDir.toFile).lazyLines_!
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
      tmpDir: Path,
      fec: Map[String, String]
    )(
      implicit
      config: Config
    ): Boolean = {
    // Generate the miter circuit
    val manifest = {
      io.circe.parser.parse(outputs("manifest.json")) match {
        case Left(failure) => fail(failure.message)
        case Right(json)   => json.hcursor.downField(topLevel)
      }
    }

    val clock = manifest.get[String]("clock").toOption
    val reset = manifest.get[String]("reset").toOption

    val resetStyle = manifest.get[String]("reset-style") match {
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

    val ports = manifest.downField("ports").as[ListMap[String, Port]].getOrElse(fail)
    val signals = manifest.downField("signals").as[ListMap[String, Signal]].getOrElse(fail)

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

    val miterFile = tmpDir.resolve("__miter.v").toFile
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
    mpw.write("  golden golden_u (\n")
    vInputs foreach { name => mpw.write(s"    .$name($name),\n") }
    vOutputs.iterator.zipWithIndex foreach {
      case (name, idx) =>
        val end = if (idx < vOutputs.size - 1) ",\n" else "\n"
        mpw.write(s"    .$name(golden__$name)$end")
    }
    mpw.write("  );\n")
    mpw.write("\n")
    mpw.write("  // Alogic instance\n")
    mpw.write("  alogic alogic_u (\n")
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

    // Write sby script
    val scriptFile = tmpDir.resolve("equiv.sby").toFile
    writeFile(scriptFile) {
      s"""[options]
         |mode ${fec.getOrElse("mode", if (clock.isDefined) "prove" else "bmc")}
         |depth ${fec.getOrElse("depth", if (clock.isDefined) 20 else 2)}
         |timeout ${fec.getOrElse("timeout", "10")}
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
         |read_verilog ${tmpDir.resolve(topLevel + ".v").toFile.getName}
         |rename $topLevel alogic
         |
         |# Read the miter circuit
         |read_verilog -formal ${miterFile.getName}
         |
         |# Resolve and prep
         |hierarchy -check -libdir $tmpDir -top miter
         |prep -run coarse: -top miter
         |
         |# Set initial state of all memories to 0
         |setparam -set INIT 0 t:$$mem
         |
         |#memory; flatten; opt -full
         |#show -colors 1 -stretch miter
         |
         |[files]
         |${topLevel + ".v"}
         |${goldenFile.getName}
         |${miterFile.getName}
         |""".stripMargin
    }

    // Perform the equivalence check
    val sbyCmd = s"$symbiyosys ${scriptFile.getName}"
    val path = s"$symbiyosysPath:${scala.util.Properties.envOrElse("PATH", "")}"
    system(sbyCmd, tmpDir, "sby-equiv.log", failOk = true, Map("PATH" -> path))
  }

  def formalEquivalenceCheck(
      topLevel: String,
      fec: Map[String, String],
      tmpDir: Path
    )(
      implicit
      config: Config
    ): Unit = {
    // Write all files to temporary directory
    writeOutputs(tmpDir)

    // Write the golden model
    val goldenFile = tmpDir.resolve("__golden.v").toFile
    writeFile(goldenFile)(fec("golden"))

    // Lint the golden model, just to be sure, as yosys "helpfully" provides
    // implicit declarations of undeclared names.
    system(
      s"$verilator --lint-only -Wall -Wno-DECLFILENAME -Wno-UNUSED ${goldenFile.getName}",
      tmpDir,
      "verilator-golden-lint.log"
    )

    // Now strip `line because it's too much for yosys...
    system(s"sed -i s/`line.*// ${goldenFile.getName}", tmpDir, "sed.log")

    // First check the simple  if that fails try the hard way
    lazy val easyOK = yosysEquiv(topLevel, goldenFile, tmpDir)
    lazy val hardOK = symbiYosysEquiv(topLevel, goldenFile, tmpDir, fec)

    if (!easyOK && !hardOK) {
      fail("FEC failed");
    }
  }

  sealed private trait MessageSpec {
    val file: String
    val line: Int
    val patterns: List[String]

    def matches(message: Message)(implicit cc: CompilerContext): Boolean = {
      val typeMatches = (this, message) match {
        case (_: WarningSpec, _: Warning) => true
        case (_: ErrorSpec, _: Error)     => true
        case (_: FatalSpec, _: Fatal)     => true
        case _                            => false
      }
      typeMatches && // Right type
      message.loc.line == line && // Right line number
      file.r.pattern.matcher(message.loc.file).matches() && // Right file pattern
      allMatches(patterns, message.msg) // Right text
    }

    def string: String = {
      val kindString = this match {
        case _: WarningSpec => "WARNING"
        case _: ErrorSpec   => "ERROR"
        case _: FatalSpec   => "FATAL"
      }
      val prefix = s"$file:$line: $kindString: "
      val triplets =
        LazyList.continually(prefix) lazyZip ("" +: LazyList.continually("... ")) lazyZip patterns
      triplets map { case (a, b, c) => a + b + c } mkString "\n"
    }

  }

  private case class WarningSpec(file: String, line: Int, patterns: List[String])
      extends MessageSpec
  private case class ErrorSpec(file: String, line: Int, patterns: List[String]) extends MessageSpec
  private case class FatalSpec(file: String, line: Int, patterns: List[String]) extends MessageSpec

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
    val mesgMatcher = """(.*):(\d+): (WARNING|ERROR|FATAL): (\.\.\. )?(.*)""".r

    val mesgSpecs = new ListBuffer[MessageSpec]

    val mesgBuff = new ListBuffer[String]
    var mesgType = ""
    var mesgFile = ""
    var mesgLine = ""

    def finishMessageSpec(): Unit = {
      val file = if (mesgFile.isEmpty) ".*" + checkFile.split("/").last else mesgFile
      mesgType match {
        case "WARNING" => mesgSpecs append WarningSpec(file, mesgLine.toInt, mesgBuff.toList)
        case "ERROR"   => mesgSpecs append ErrorSpec(file, mesgLine.toInt, mesgBuff.toList)
        case "FATAL"   => mesgSpecs append FatalSpec(file, mesgLine.toInt, mesgBuff.toList)
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

    val source = Source.fromFile(checkFile)

    try {
      for {
        (line, lineNo) <- source.getLines zip LazyList.from(1)
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
          case mesgMatcher(file, line, kind, null, pattern) if key == "" =>
            if (mesgBuff.nonEmpty) {
              finishMessageSpec()
            }
            mesgFile = file.trim
            mesgLine = line.trim
            mesgType = kind.trim
            mesgBuff append pattern
          case mesgMatcher(file, line, kind, _, pattern) if key == "" =>
            assert(mesgFile == file.trim, "Message continuation must have same file pattern")
            assert(mesgLine == line.trim, "Message continuation must have same line number")
            assert(mesgType == kind.trim, "Message continuation must have same message type")
            mesgBuff append pattern
          case _ =>
        }
      }
    } finally {
      source.close()
    }

    if (mesgBuff.nonEmpty) {
      finishMessageSpec()
    }

    (attrs.toMap, dicts.toMap, mesgSpecs.toList)
  } tap {
    case (attr, dict, _) =>
      val valid = Set(
        "expect-file",
        "fec",
        "ignore",
        "no-assertions",
        "no-reset-all",
        "out-top",
        "output-name-max-length",
        "reset-style",
        "sim",
        "top",
        "verilator-lint-off"
      )
      attr.keysIterator concat dict.keysIterator foreach { k =>
        if (!valid(k)) {
          fail(s"Unknown test attribute '$k'")
        }
      }
  }

  def defineTest(
      name: String,
      searchPath: File,
      top: String,
      checkFile: String
    ): Unit = {
    name in { configMap: ConfigMap =>
      implicit val config: Config = Config(
        dumpTrees = configMap.getWithDefault("dump-trees", "0").toInt != 0,
        tmpDir = configMap.getOptional[String]("tmpdir"),
        trace = configMap.getWithDefault("trace", "0").toInt != 0,
        traceElaborate = configMap.getWithDefault("trace-elaborate", "0").toInt != 0,
        verbose = configMap.getWithDefault("verbose", "0").toInt
      )

      // Parse the check file
      val (attr, dict, messageSpecs) = parseCheckFile(checkFile)

      // Cancel test if required
      if (attr contains "ignore") {
        cancel
      }

      val resetStyle = attr.get("reset-style") map {
        case "async-low"  => ResetStyle.AsyncLow
        case "async-high" => ResetStyle.AsyncHigh
        case "sync-low"   => ResetStyle.SyncLow
        case "sync-high"  => ResetStyle.SyncHigh
        case other        => fail(s"Unknown reset style: $other")
      } getOrElse ResetStyle.SyncHigh

      val resetAll = !(attr contains "no-reset-all")

      // Create compiler context
      implicit val cc: CompilerContext = new CompilerContext(
        Settings(
          moduleSearchDirs = List(searchPath),
          outputWriterFactory = outputWriterFactory,
          messageEmitter = messageEmitter,
          dumpTrees = config.dumpTrees,
          resetStyle = resetStyle,
          resetAll = resetAll,
          traceElaborate = config.traceElaborate,
          outputNameMaxLength = attr.get("output-name-max-length") map { _.toInt },
          assertions = !(attr contains "no-assertions")
        )
      )

      // Do the compilation
      try {
        cc.compile(List(attr.getOrElse("top", top)))
      } catch {
        case e: InternalCompilerErrorException =>
          print(e.message.string)
          throw e
        case e: StackOverflowError =>
          // For some reason scalatest swallows stack overflow exceptions,
          // wrapping them makes them show up in the test runner.......
          throw new RuntimeException(e)
      } finally {
        if (cc.settings.dumpTrees) {
          withTmpDir(writeOutputs)
        }
      }

      // Check messages
      {
        // fail flag
        var messageCheckFailed = false

        // Group messages by specs
        val messageGroups = messages.toList groupBy { message =>
          messageSpecs find { _ matches message }
        }

        // Fail if a spec matches multiple messages
        for ((Some(spec), messages) <- messageGroups if messages.lengthIs > 1) {
          println("Message pattern:")
          println(spec.string)
          println("Matches multiple messages:")
          messages foreach { message =>
            println(message.string)
          }
          messageCheckFailed = true
        }

        // Print unexpected messages
        messageGroups.get(None) foreach { unexpectedMessages =>
          unexpectedMessages foreach { message =>
            println("Unexpected message:")
            println(message.string)
          }
          messageCheckFailed = true
        }

        // Print unused patterns
        for {
          spec <- messageSpecs
          if !(messageGroups contains Some(spec))
        } {
          println("Unused message pattern:")
          println(spec.string)
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
      cc.hasError shouldBe expectedToFail

      if (!expectedToFail) {
        attr get "expect-file" foreach { name =>
          // Check expected output file exists
          checkFileExists(name)
        }

        val outTop = attr.getOrElse("out-top", top)

        dict get "sim" match {
          // Build and run testbench if provided
          case Some(sim) =>
            withTmpDir { tmpDir =>
              val test = sim("test")
              val expect = sim.getOrElse("expect", "")
              val dpi = sim.getOrElse("dpi", "")
              val timeout = sim.getOrElse("timeout", "100").toLong
              val trace = config.trace
              verilatorSim(outTop, test, expect, dpi, timeout, trace, tmpDir)
            }
          case None =>
            // Lint unless told not to
            if (!(attr contains "verilator-lint-off")) {
              withTmpDir { tmpDir =>
                verilatorLint(outTop, tmpDir)
              }
            }
        }

        dict get "fec" foreach { fec =>
          // Perform equivalence check with golden reference
          withTmpDir { tmpDir =>
            formalEquivalenceCheck(outTop, fec, tmpDir)
          }
        }
      }
    }
  }

}
