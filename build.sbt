////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// About the project
////////////////////////////////////////////////////////////////////////////////

name := "alogic"

organization := "Argon Design"

////////////////////////////////////////////////////////////////////////////////
// Scala compiler
////////////////////////////////////////////////////////////////////////////////

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-deprecation",
                      "-feature",
                      "-explaintypes",
                      "-unchecked",
                      "-Xlint:_")

////////////////////////////////////////////////////////////////////////////////
// Library dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.12" % "0.8.0"

libraryDependencies += "org.rogach" %% "scallop" % "3.1.2"

////////////////////////////////////////////////////////////////////////////////
// Testing dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

logBuffered in Test := false

testOptions in Test += Tests.Argument("-oD") // Add F for full stack traces

////////////////////////////////////////////////////////////////////////////////
// Style check
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Antlr4 plugin
////////////////////////////////////////////////////////////////////////////////

enablePlugins(Antlr4Plugin)

antlr4Version in Antlr4 := "4.7.1"

antlr4PackageName in Antlr4 := Some("com.argondesign.alogic.antlr")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := true

////////////////////////////////////////////////////////////////////////////////
// SBT native packager
////////////////////////////////////////////////////////////////////////////////

enablePlugins(JavaAppPackaging)

stage := (stage dependsOn (test in Test)).value

bashScriptExtraDefines += """
# Pass a secret option if stderr is tty
if [[ -t 2 ]]; then
  stderrisatty="--stderrisatty"
fi

# Prepend '--' to the command line arguments. This in fact causes the wrapper
# to not consume any arguments, in particular -D options
set -- -- ${stderrisatty} "$@"
"""

////////////////////////////////////////////////////////////////////////////////
// SBT git
////////////////////////////////////////////////////////////////////////////////

enablePlugins(GitVersioning)

git.useGitDescribe := true

////////////////////////////////////////////////////////////////////////////////
// SBT buildinfo
////////////////////////////////////////////////////////////////////////////////

enablePlugins(BuildInfoPlugin)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)

buildInfoPackage := "com.argondesign.alogic"
