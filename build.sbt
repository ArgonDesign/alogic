////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// General
////////////////////////////////////////////////////////////////////////////////

name := "alogic"

organization := "alogic"

version := "1"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint:_")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.11.5"
libraryDependencies += "com.beachape.filemanagement" %% "schwatcher" % "0.3.2"
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3"
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
libraryDependencies += "org.rogach" %% "scallop" % "2.1.1"

////////////////////////////////////////////////////////////////////////////////
// Antlr4 plugin
////////////////////////////////////////////////////////////////////////////////

antlr4Settings

antlr4PackageName in Antlr4 := Some("alogic.antlr")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := true

////////////////////////////////////////////////////////////////////////////////
// ScalaTest
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

logBuffered in Test := false

////////////////////////////////////////////////////////////////////////////////
// SBT Native packager
////////////////////////////////////////////////////////////////////////////////

enablePlugins(JavaAppPackaging)

// Prepend '--' to the command line arguments in the wrapper script.
// This in fact causes the wrapper script to not consume any arguments,
// in particular -D options
bashScriptExtraDefines += """set -- -- "$@""""
