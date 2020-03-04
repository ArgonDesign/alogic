////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler CLI
// Author: Geza Lore
//
// DESCRIPTION:
//
// Command line option parser
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import java.io.File

import com.argondesign.alogic.core.enums.ResetStyle
import com.argondesign.alogic.core.enums.UninitializedLocals
import com.argondesign.alogic.util.PartialMatch
import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopConf
import org.rogach.scallop.ScallopOption
import org.rogach.scallop.ValueConverter
import org.rogach.scallop.singleArgConverter

// Option parser based on Scallop. See the Scallop wiki for usage:
// https://github.com/scallop/scallop/wiki
class CLIConf(args: Seq[String]) extends ScallopConf(args) with PartialMatch {
  private[this] implicit val fileConverter =
    singleArgConverter[File](path => (new File(path)).getCanonicalFile())

  // Ensures all option instances have only a single argument
  // eg -I foo -I bar -I baz, but not -I foo bar
  private[this] val singlefileListConverter = new ValueConverter[List[File]] {

    def parse(instances: List[(String, List[String])]) = {
      val bad = instances.filter(_._2.size > 1)
      if (!bad.isEmpty) {
        val msg: List[String] = "Only one argument can be provided for each " +
          s"instance of option '${bad.head._1}'. Provided:" :: (for ((_, r) <- bad)
          yield r mkString " ");
        Left(msg mkString "\n")
      } else {
        Right(Some(instances.flatMap(_._2) map { path =>
          (new File(path)).getCanonicalFile()
        }))
      }
    }
    val argType = ArgType.SINGLE
  }

  private[this] def validateOption[T](
      option: ScallopOption[T]
  )(
      check: PartialFunction[T, String]
  ) = addValidation {
    option.toOption flatMap check.lift map { Left(_) } getOrElse Right(())
  }

  private[this] def validateListOption[T](
      option: ScallopOption[List[T]]
  )(
      check: PartialFunction[T, String]
  ) = addValidation {
    val msgs = option.toOption.getOrElse(Nil) collect check
    if (msgs.nonEmpty) {
      Left(msgs mkString ("\n", "\n", ""))
    } else {
      Right(())
    }
  }

  private[this] def validateFileExist(option: ScallopOption[File]) = {
    validateOption(option) {
      case path: File if !path.exists() => s"'${path}' does not exist"
    }
  }

  private[this] def validateFileIsRegular(option: ScallopOption[File]) = {
    validateOption(option) {
      case path: File if !path.isFile() => s"'${path}' is not a regular file"
    }
  }

  private[this] def validateFilesExist(option: ScallopOption[List[File]]) = {
    validateListOption(option) {
      case path: File if !path.exists() => s"'${path}' does not exist"
    }
  }

  private[this] def validateFilesAreDirectories(option: ScallopOption[List[File]]) = {
    validateListOption(option) {
      case path: File if !path.isDirectory() => s"'${path}' is not a directory"
    }
  }

  private[this] def validateOneOf[T](option: ScallopOption[T])(choices: T*) = addValidation {
    option.toOption partialMatch {
      case Some(value) if !(choices contains value) => {
        Left(s"Option '${option.name}' must be one of: " + (choices mkString " "))
      }
    } getOrElse {
      Right(())
    }
  }

  version(BuildInfo.version)

  banner("Alogic compiler")

  errorMessageHandler = { message =>
    Console.err.println(s"FATAL: %s" format message)
    sys.exit(1)
  }

  val ydir = opt[List[File]](
    short = 'y',
    descr = "Directory to search for entities"
  )(singlefileListConverter)
  validateFilesExist(ydir)
  validateFilesAreDirectories(ydir)

  val incdir = opt[List[File]](
    short = 'I',
    descr = "Directory to search for includes"
  )(singlefileListConverter)
  validateFilesExist(incdir)
  validateFilesAreDirectories(incdir)

  val odir = opt[File](
    short = 'o',
    required = true,
    descr = "Output directory. See description of --srcbase as well"
  )

  val defs = props[String](
    name = 'D',
    keyName = "name",
    descr = "Predefine preprocessor macro"
  )

  val srcbase = opt[File](
    noshort = true,
    required = false,
    descr = """|Base directory for source files. When specified, all directories
               |specified with -y must be under this directory, and output files
               |will be written to the same relative path under the output
               |directory specified with -o, as the corresponding source is
               |relative to --srcbase. When --srcbase is not provided, output
               |files are written to the output directory directly
               |""".stripMargin.replace('\n', ' ')
  )

  validateOpt(srcbase, ydir) {
    case (Some(base), Some(ys)) => {
      val basePath = base.toPath.toRealPath()
      val bad = ys filterNot { _.toPath.toRealPath() startsWith basePath }
      if (bad.isEmpty) {
        Right(())
      } else {
        val msgs = for (file <- bad) yield s"-y '${file}' is not under --srcbase '${base}'"
        Left(msgs mkString "\n")
      }
    }
    case _ => Right(())
  }

  val sep = opt[String](
    noshort = true,
    required = false,
    descr = "Structure field separator sequence used in the output. Default is '__'.",
    default = Some("__")
  )

  val uninitialized = opt[UninitializedLocals.Type](
    noshort = true,
    required = false,
    descr = """|Specify whether to default initialize local variables declared
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
      }, {
        case _ => Left("must be one of 'none', 'zeros', 'ones', 'random")
      }
    )
  )

  val ensurePrefix = opt[String](
    name = "ensure-prefix",
    noshort = true,
    required = false,
    descr = """|Ensure all output module names start with the prefix provided.
               |If the name of an Alogic entity already starts with a suffix of
               |the given prefix, only the remaining initial part of the prefix
               |will be applied.
               |""".stripMargin.replace('\n', ' '),
    default = Some("")
  )

  val header = opt[File](
    noshort = true,
    required = false,
    descr = "File containing text that will be prepended to every output file"
  )
  validateFileExist(header)
  validateFileIsRegular(header)

  val color = opt[String](
    noshort = true,
    required = false,
    descr = """|Colorize diagnostic messages, one of: 'always|never|auto'.
               |Default is 'auto' which uses colors only if the output is
               |to a terminal
               |""".stripMargin.replace('\n', ' '),
    default = Some("auto")
  )
  validateOneOf(color)("always", "never", "auto")

  val moduleManifest = opt[File](
    name = "module-manifest",
    noshort = true,
    required = false,
    descr = "Write generated module manifest to file"
  )

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
    descr = """|Determines the reset style used in the output. One of
               |'async-low', 'async-high', 'sync-low', 'sync-high' for
               |synchronous/asynchronous assert, active low/high reset.
               |Default is 'async-low
               |""".stripMargin.replace('\n', ' '),
    default = Some(ResetStyle.AsyncLow)
  )(
    singleArgConverter(
      {
        case "async-low"  => ResetStyle.AsyncLow
        case "async-high" => ResetStyle.AsyncHigh
        case "sync-low"   => ResetStyle.SyncLow
        case "sync-high"  => ResetStyle.SyncHigh
      }, {
        case _ => Left("must be one of 'async-low', 'async-high', 'sync-low', 'sync-high")
      }
    )
  )

  val noResetAll = opt[Boolean](
    noshort = true,
    descr = """|Only reset flops that require reset initialization according
               |to Alogic semantics, and leave other flops unreset. By default
               |all flops emitted are reset.
               |""".stripMargin.replace('\n', ' ')
  )

  val genLoopLimit = opt[Int](
    noshort = true,
    descr = """|Maximum iteration count of standard 'gen for' loops before
               |assuming the loop is infinite.
               |""".stripMargin.replace('\n', ' '),
    default = Some(1024)
  )

  // There is no standard library call to check if the console is a terminal,
  // so we pass this hidden option from the wrapper script to help ourselves out
  val stderrisatty = toggle(noshort = true, hidden = true)

  // Dump entities after each pass
  val dumpTrees = toggle(name = "dump-trees", noshort = true, hidden = true)

  // Randomly shuffle Ent nodes
  val shuffleEnts = opt[Int](noshort = true, default = None, hidden = true)

  val toplevel = trailArg[List[String]](
    required = true,
    descr = "List of top level entity names"
  )

  verify()
}
