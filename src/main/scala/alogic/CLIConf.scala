////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import org.rogach.scallop.ScallopConf
import org.rogach.scallop.listArgConverter
import org.rogach.scallop.singleArgConverter
import org.rogach.scallop.propsConverter

import scalax.file.Path
import org.rogach.scallop.ArgType
import org.rogach.scallop.ValueConverter

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
      Left(s"Input '${path.path}' does not exist.")
    else
      Right(Unit)
  }

  def pathListValidator(pathList: List[Path]): Either[String, Unit] = {
    val messages = for (path <- pathList if path.nonExistent) yield {
      s"Input '${path.path}' does not exist."
    }

    if (messages.isEmpty) Right(Unit) else Left(messages mkString ("\n", "\n", ""))
  }

  printedName = "alogic"

  banner("Alogic compiler")

  val odir = opt[Path](
    short = 'o',
    default = Some(Path.fromString("generated")),
    descr = "Output directory")

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

  val path = trailArg[Path](
    required = true,
    descr = "Source file or source directory")
  validate(path)(pathValidator)

  verify()
}
