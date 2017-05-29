package alogic

import org.rogach.scallop.ScallopConf
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

  val headers = trailArg[List[Path]](
    required = false,
    descr = "header files")
  validate(headers)(pathListValidator)

  val ipath = trailArg[Path](
    required = true,
    descr = "Source file or source directory")
  validate(ipath)(pathValidator)

  verify()
}
