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

import org.rogach.scallop.ArgType
import org.rogach.scallop.ScallopConf
import org.rogach.scallop.ScallopOption
import org.rogach.scallop.ValueConverter
import org.rogach.scallop.singleArgConverter

// Option parser based on Scallop. See the Scallop wiki for usage:
// https://github.com/scallop/scallop/wiki
class CLIConf(args: Seq[String]) extends ScallopConf(args) {
  private[this] implicit val fileConverter = singleArgConverter[File](path => (new File(path)).getCanonicalFile())

  // Ensures all option instances have only a single argument
  // eg -I foo -I bar -I baz, but not -I foo bar
  private[this] val singlefileListConverter = new ValueConverter[List[File]] {
    def parse(instances: List[(String, List[String])]) = {
      val bad = instances.filter(_._2.size > 1)
      if (!bad.isEmpty) {
        val msg: List[String] = "Only one argument can be provided for each " +
          s"instance of option '${bad.head._1}'. Provided:" :: (for ((_, r) <- bad) yield r mkString " ");
        Left(msg mkString "\n")
      } else {
        Right(Some(instances.flatMap(_._2) map { path => (new File(path)).getCanonicalFile() }))
      }
    }
    val argType = ArgType.SINGLE
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

  private[this] def validateFilesExist(option: ScallopOption[List[File]]) = validateListOption(option) {
    case path: File if !path.exists() => s"'${path}' does not exist"
  }

  private[this] def validateFilesAreDirectories(option: ScallopOption[List[File]]) = validateListOption(option) {
    case path: File if !path.isDirectory() => s"'${path}' is not a directory"
  }

  printedName = "alogic"

  banner("Alogic compiler")

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
    short    = 'o',
    required = true,
    descr    = "Output directory"
  )

  val defs = props[String](
    name    = 'D',
    keyName = "name",
    descr   = "Predefine preprocessor macro"
  )

  val toplevel = trailArg[String](
    required = true,
    descr    = "Name of top level entity"
  )

  verify()
}
