////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//  Command line option parser
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.enums.UninitializedLocals
import com.argondesign.alogic.util.PartialMatch
import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopConf
import org.rogach.scallop.ScallopOption
import org.rogach.scallop.ValueConverter
import org.rogach.scallop.flagConverter
import org.rogach.scallop.intConverter
import org.rogach.scallop.optDefault
import org.rogach.scallop.singleArgConverter
import org.rogach.scallop.stringConverter

import java.io.File
import java.nio.file.Path
import scala.util.chaining.scalaUtilChainingOps
import scala.util.Try

// Option parser based on Scallop. See the Scallop wiki for usage:
// https://github.com/scallop/scallop/wiki
class OptionParser(args: Seq[String], messageBuffer: MessageBuffer, sandboxPathOpt: Option[Path])
    extends ScallopConf(args)
    with PartialMatch {

  object NotInSanbox extends Exception

  def stringToPath(input: String): Path = {
    val f = new File(input)
    sandboxPathOpt pipe {
      case Some(sandboxPath) if !f.isAbsolute => (sandboxPath resolve input).toFile
      case _                                  => f
    } pipe {
      _.getCanonicalFile.toPath
    } tap { path =>
      sandboxPathOpt foreach { sandboxPath =>
        if (!path.startsWith(sandboxPath)) {
          throw NotInSanbox
        }
      }
    }
  }

  implicit private val pathConverter: ValueConverter[Path] =
    singleArgConverter(
      stringToPath,
      { case NotInSanbox => Left(s"not inside sandbox") }
    )

  // Ensures all option instances have only a single argument
  // eg -y foo -y bar -y baz, but not -y foo bar

  abstract class SingleValueCovnerter[T] extends ValueConverter[List[T]] {

    protected def convert(value: String): T

    protected val handler: PartialFunction[Throwable, Either[String, Option[List[T]]]] = {
      case t: Throwable => throw t
    }

    def parse(instances: List[(String, List[String])]): Either[String, Option[List[T]]] = {
      val bad = instances.filter(_._2.size > 1)
      if (bad.nonEmpty) {
        val msg: List[String] = "Only one argument can be provided for each " +
          s"instance of option '${bad.head._1}'. Provided:" :: (for ((_, r) <- bad)
          yield r mkString " ");
        Left(msg mkString "\n")
      } else {
        Try(Right(Some(instances.flatMap(_._2) map convert)))
          .recover(handler)
          .recover({ case _: Exception => Left("wrong arguments format") })
          .get
      }
    }

    val argType: ArgType.V = ArgType.SINGLE
  }

  private val singlePathListConverter: SingleValueCovnerter[Path] = new SingleValueCovnerter[Path] {
    final override protected def convert(input: String): Path = stringToPath(input)

    final override protected val handler = {
      case NotInSanbox => Left("not inside sandbox")
    }

  }

  private val singleStringListconverter: SingleValueCovnerter[String] = identity(_)

  private def validateOption[T](
      option: ScallopOption[T]
    )(
      check: PartialFunction[T, String]
    ): Unit = addValidation {
    option.toOption flatMap check.lift map {
      Left(_)
    } getOrElse Right(())
  }

  private def validateListOption[T](
      option: ScallopOption[List[T]]
    )(
      check: PartialFunction[T, String]
    ): Unit = addValidation {
    val msgs = option.toOption.getOrElse(Nil) collect check
    if (msgs.nonEmpty) {
      Left(msgs mkString "\n")
    } else {
      Right(())
    }
  }

  private def validatePathExist(option: ScallopOption[Path]): Unit =
    validateOption(option) {
      case path: Path if !path.toFile.exists() => s"'$path' does not exist"
    }

  private def validatePathsExist(option: ScallopOption[List[Path]]): Unit =
    validateListOption(option) {
      case path: Path if !path.toFile.exists() => s"'$path' does not exist"
    }

  private def validatePathIsRegularFile(option: ScallopOption[Path]): Unit =
    validateOption(option) {
      case path: Path if !path.toFile.isFile => s"'$path' is not a regular file"
    }

  private def validatePathsAreDirectories(option: ScallopOption[List[Path]]): Unit =
    validateListOption(option) {
      case path: Path if !path.toFile.isDirectory => s"'$path' is not a directory"
    }

  private def validateOneOf[T](option: ScallopOption[T])(choices: T*): Unit = addValidation {
    option.toOption partialMatch {
      case Some(value) if !(choices contains value) =>
        Left(s"Option '${option.name}' must be one of: " + (choices mkString " "))
    } getOrElse {
      Right(())
    }
  }

  private def validateOutputNameMaxLengthWithPrefix(
      outNameMaxLen: ScallopOption[Int],
      prefix: ScallopOption[String]
    ): Unit =
    addValidation {
      val min = prefix().length + 16
      outNameMaxLen.toOption partialMatch {
        case Some(value) if value < min =>
          Left(s"""Minimum value of option '${outNameMaxLen.name}' is $min
                  |(prefix length + 16) but value provided is $value
                  |""".stripMargin.replaceAll("\n+", " "))
      } getOrElse Right(())
    }

  version(BuildInfo.version)

  banner("Alogic compiler")

  errorMessageHandler = { message =>
    messageBuffer.error(Loc.unknown, message)
  }

  val ydir = opt[List[Path]](
    short = 'y',
    descr = """|Directory to search for imported packages. Can be repeated to
               |specify multiple search paths. If none provided, the directory
               |containing the input file is assumed.
               |""".stripMargin.replace('\n', ' ')
  )(singlePathListConverter)

  validatePathsExist(ydir)
  validatePathsAreDirectories(ydir)

  val odir = opt[Path](
    short = 'o',
    required = true,
    descr = "Names the output directory."
  )

  val sep = opt[String](
    noshort = true,
    required = false,
    descr = "Hierarchical name separator sequence used in the output. Default is '__'.",
    default = Some("__")
  )

  val uninitialized = opt[UninitializedLocals.Type](
    noshort = true,
    required = false,
    descr = """Specify whether to default initialize local variables declared
              |without an explicit initializer expression. Possible values
              |are: 'none' meaning leave them un-initialized, 'zeros' means
              |initialize them to zero. 'ones' means initialize them to all
              |ones, 'random' means initialize them to a compile time constant,
              |deterministic, but otherwise arbitrary bit pattern. Default is
              |'none'
              |""".stripMargin.replace('\n', ' '),
    default = Some(UninitializedLocals.None)
  )(
    singleArgConverter(
      {
        case "none"   => UninitializedLocals.None
        case "zeros"  => UninitializedLocals.Zeros
        case "ones"   => UninitializedLocals.Ones
        case "random" => UninitializedLocals.Random
      },
      {
        case _ => Left("must be one of 'none', 'zeros', 'ones', 'random'")
      }
    )
  )

  val ensurePrefix = opt[String](
    name = "ensure-prefix",
    noshort = true,
    required = false,
    descr = """Ensure all output module names start with the prefix provided.
              |If the name of an Alogic entity already starts with a suffix of
              |the given prefix, only the remaining initial part of the prefix
              |will be applied.
              |""".stripMargin.replace('\n', ' '),
    default = Some("")
  )

  val outputNameMaxLength = opt[Int](
    name = "output-name-max-length",
    noshort = true,
    required = false,
    descr = """Enforce a maximum length upon output module names (excluding top-level
              |modules). Any names exceeding this length will be truncated and appended
              |with a short number. The default behaviour is no maximum.
              |""".stripMargin.replace('\n', ' '),
    default = None
  )

  validateOutputNameMaxLengthWithPrefix(outputNameMaxLength, ensurePrefix)

  val header = opt[Path](
    noshort = true,
    required = false,
    descr = "File containing text that will be prepended to every output file"
  )

  validatePathExist(header)
  validatePathIsRegularFile(header)

  val color = opt[String](
    noshort = true,
    required = false,
    descr = """Colorize diagnostic messages, one of: 'always|never|auto'.
              |Default is 'auto' which uses colors only if the output is
              |to a terminal
              |""".stripMargin.replace('\n', ' '),
    default = Some("auto")
  )

  validateOneOf(color)("always", "never", "auto")

  // --compiler-deps is implemented in the wrapper.
  // It is defined here so it appears in --help
  val compilerDeps = opt[Boolean](
    name = "compiler-deps",
    noshort = true,
    default = Some(false),
    descr = "Print compiler dependencies and exit"
  )

  val resetStyle = opt[ResetStyle.Type](
    noshort = true,
    required = false,
    descr = """Determines the reset style used in the output. One of
              |'async-low', 'async-high', 'sync-low', 'sync-high' for
              |asynchronous/synchronous assert, active low/high reset.
              |Default is 'async-low'
              |""".stripMargin.replace('\n', ' '),
    default = Some(ResetStyle.AsyncLow)
  )(
    singleArgConverter(
      {
        case "async-low"  => ResetStyle.AsyncLow
        case "async-high" => ResetStyle.AsyncHigh
        case "sync-low"   => ResetStyle.SyncLow
        case "sync-high"  => ResetStyle.SyncHigh
      },
      {
        case _ => Left("must be one of 'async-low', 'async-high', 'sync-low', 'sync-high")
      }
    )
  )

  val noResetAll = opt[Boolean](
    noshort = true,
    descr = """Only reset flops that require reset initialization according
              |to Alogic semantics, and leave other flops unreset. By default
              |all flops emitted are reset.
              |""".stripMargin.replace('\n', ' ')
  )

  val genLoopLimit = opt[Int](
    noshort = true,
    descr = """Maximum iteration count of standard 'gen for' loops before
              |assuming the loop is infinite.
              |""".stripMargin.replace('\n', ' '),
    default = Some(1024)
  )

  val combRecLimit = opt[Int](
    noshort = true,
    descr = """Combinational function recursion limit. This is the maximum
              |number of calls to the same function that can be active before
              |the compiler assumes the recursion is infinite and reports an
              |error.
              |""".stripMargin.replace('\n', ' '),
    default = Some(16)
  )

  val noAssertions = opt[Boolean](
    noshort = true,
    descr = "Disable emitting assertions"
  )

  val stats = opt[Boolean](
    noshort = true,
    descr = "Emit statistics about the design"
  )

  val param = opt[List[String]](
    short = 'P',
    descr = """|Specifies actual parameters to the input file.
               |""".stripMargin.replace('\n', ' ')
  )(singleStringListconverter)

  // Note: Setting the thread count is implemented in the wrapper
  val parallelism = opt[Int](
    short = 'j',
    argName = "N",
    descr = """Compile in parallel, on up to N threads. If N is omitted,
              |the number of available processor is used""".stripMargin
  )(optDefault(Int.MaxValue))

  validateOption(parallelism) {
    case n if n <= 0 => "parallelism option must be >= 1"
  }

  val schematic = opt[Boolean](
    noshort = true,
    descr = "Emit schematic diagram of each top level entity"
  )

  // There is no standard library call to check if the console is a terminal,
  // so we pass this hidden option from the wrapper script to help ourselves out
  val stderrisatty = toggle(noshort = true, hidden = true)

  // Dump entities after each pass
  val dumpTrees = toggle(name = "dump-trees", noshort = true, hidden = true)

  // Measure and report inserted execution timing
  val profile = toggle(name = "profile", noshort = true, hidden = true)

  // Crash the compiler for testing purposes
  val testCrash = toggle(name = "test-crash", noshort = true, hidden = true)

  validateOption(testCrash) {
    case true => throw new RuntimeException("Crashing on purpose due to --test-crash")
  }

  // Input file
  val file = trailArg[Path](
    required = true,
    descr = "Input file"
  )

  validatePathExist(file)
  validatePathIsRegularFile(file)

  verify()
}
