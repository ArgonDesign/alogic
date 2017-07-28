////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopConf
import org.rogach.scallop.ValueConverter
import org.rogach.scallop.listArgConverter
import org.rogach.scallop.singleArgConverter

import scalax.file.Path

/**
 * Option parser based on Scallop. See the Scallop wiki for usage:
 *  https://github.com/scallop/scallop/wiki
 */
class CLIConf(args: Seq[String]) extends ScallopConf(args) {
  implicit val pathCovnert = singleArgConverter[Path](Path.fromString(_))
  implicit val pathListConverter = listArgConverter[Path](Path.fromString(_))

  // Ensures all option instances have only a single argument
  // eg -I foo -I bar -I baz, but not -I foo bar
  val singlePathListConverter = new ValueConverter[List[Path]] {
    def parse(instances: List[(String, List[String])]) = {
      val bad = instances.filter(_._2.size > 1)
      if (!bad.isEmpty) {
        val msg: List[String] = "Only one argument can be provided for each " +
          s"instance of option '${bad.head._1}'. Provided:" :: (for ((_, r) <- bad) yield r mkString " ");
        Left(msg mkString "\n")
      } else {
        Right(Some(instances.flatMap(_._2) map Path.fromString))
      }
    }
    val tag = scala.reflect.runtime.universe.typeTag[List[Path]] // Magic to make typing work
    val argType = ArgType.SINGLE
  }

  def pathValidator(path: Path): Either[String, Unit] = {
    if (path.nonExistent)
      Left(s"'${path.path}' does not exist.")
    else
      Right(Unit)
  }

  def pathListValidator(pathList: List[Path]): Either[String, Unit] = {
    (pathList map pathValidator).fold(Right[String, Unit](Unit)) { (a, b) =>
      (a, b) match {
        case (Left(a), Left(b))               => Left(a + "\n" + b)
        case (_: Right[_, _], l: Left[_, _])  => l
        case (l: Left[_, _], _: Right[_, _])  => l
        case (_: Right[_, _], _: Right[_, _]) => Right(Unit)
      }
    }
  }

  printedName = "alogic"

  banner("Alogic compiler")

  val odir = opt[Path](
    short = 'o',
    default = Some(Path.fromString("generated")),
    descr = "Output directory")

  val srcdir = opt[Path](
    noshort = true,
    default = None,
    descr = """|Root path of source search. Required if using multiple
               |input paths. If there is only a single
               |input path, then if that path is a directory, this
               |defaults to the same directory, or if the single input path
               |is a file, this defaults to the parent directory of that file.
               |""".stripMargin.replace('\n', ' '))

  val incdir = opt[List[Path]](
    short = 'I',
    descr = "Add to include search path")(singlePathListConverter)
  validate(incdir)(pathListValidator)

  val defs = props[String]('D',
    keyName = "name",
    descr = "Predefine preprocessor macro")

  val monitor = opt[Boolean](
    short = 'm',
    descr = "Recompile whenever sources change")

  // At the moment there does not seem to be much benefit
  // from multithreading, so leave it off in order that error messages are in correct order
  val parallel = opt[Boolean](
    short = 'p',
    descr = "Compile using multiple threads")

  val verbose = opt[Boolean](
    noshort = true,
    descr = "Produce more verbose messages")

  val time = opt[Int](
    noshort = true,
    argName = "N",
    hidden = true,
    descr = "Run compilation N times for benchmarking")
  validate(time) { value: Int =>
    if (value < 2) Left("Must be > 2") else Right(())
  }

  val path = trailArg[List[Path]](
    required = true,
    descr = """|Source paths (files and directories).
               |If srcdir is provided, then multiple paths relative
               |to srcdir can be specified, and all paths must be
               |under srcdir. If srcdir is not provided, then a single
               |arbitrary input path can be specified.
               |""".stripMargin.replace('\n', ' '))
  validate(path) { paths =>
    if (srcdir.isDefined) {
      if (srcdir().nonExistent) {
        Left(s"srcdir '${srcdir().path}' does not exist.")
      } else if (paths exists { _.isAbsolute }) {
        Left("Absolute input paths can only be used if using a single input path and not using --srcdir.")
      } else {
        val sdir: Path = srcdir().toRealPath()
        val absPaths: List[Path] = paths map { sdir / _ } map { _.toRealPath() }
        val outsidePaths = (absPaths zip paths) collect {
          case (absPath, path) if !(absPath startsWith sdir) => path
        }
        outsidePaths match {
          case Nil => pathListValidator(absPaths)
          case paths => {
            val msgs = paths map { path => s"Path '${path.path}' is outside specified srcdir '${srcdir().path}'" }
            Left(msgs mkString "\n")
          }
        }
      }
    } else {
      if (paths.length > 1) {
        Left("--srcdir must be provided when using more than one input paths")
      } else {
        pathListValidator(paths)
      }
    }

  }

  verify()
}
