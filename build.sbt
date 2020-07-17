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

organization := "com.argondesign"

////////////////////////////////////////////////////////////////////////////////
// Scala compiler
////////////////////////////////////////////////////////////////////////////////

scalaVersion := "2.13.2"

scalacOptions ++= Seq("-feature", "-explaintypes", "-unchecked", "-Xlint:_")

////////////////////////////////////////////////////////////////////////////////
// Library dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.rogach" %% "scallop" % "3.3.1"

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

////////////////////////////////////////////////////////////////////////////////
// Testing dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
) map {
  _ % "0.13.0" % "test"
}

logBuffered in Test := false

testOptions in Test += Tests.Argument("-oD") // Add F for full stack traces

////////////////////////////////////////////////////////////////////////////////
// Style check
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Antlr4 plugin
////////////////////////////////////////////////////////////////////////////////

enablePlugins(Antlr4Plugin)

antlr4Version in Antlr4 := "4.8"

antlr4PackageName in Antlr4 := Some("com.argondesign.alogic.antlr")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := true

////////////////////////////////////////////////////////////////////////////////
// SBT native packager
////////////////////////////////////////////////////////////////////////////////

enablePlugins(JavaAppPackaging)

// stage := (stage dependsOn (test in Test)).value

bashScriptExtraDefines +=
  """
# Pass a secret option if stderr is tty, but only if not asking for
# --help or --version
if [[ "$*" != "-h" && "$*" != "--help" ]] && \
   [[ "$*" != "-v" && "$*" != "--version" ]] && \
   [[ -t 2 ]]; then
  stderrisatty="--stderrisatty"
fi

if [[ "$*" == "--compiler-deps" ]]; then
  readlink -f "$0"
  echo "$app_classpath" | tr ":" "\n"
  exit 0
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

buildInfoKeys := Seq[BuildInfoKey](
  name,
  version,
  scalaVersion,
  sbtVersion,
  coverageEnabled
)

buildInfoPackage := "com.argondesign.alogic"

////////////////////////////////////////////////////////////////////////////////
// SBT scoverage
////////////////////////////////////////////////////////////////////////////////

coverageOutputXML := true
coverageOutputHTML := !(sys.env contains "TRAVIS")
coverageOutputCobertura := false

val coverageUpdateIgnored = taskKey[Unit]("Mark ignored statements as such")
coverageUpdateIgnored := {
  import scoverage.Serializer
  import scoverage.Coverage
  import scoverage.Statement

  // Load coverage
  val dataDir = crossTarget.value / "scoverage-data"
  val coverageFile = Serializer.coverageFile(dataDir)
  val coverage = Serializer.deserialize(coverageFile)

  // Gather code ranges that should be ignored
  val ignoreRanges = coverage.statements filter { stmt =>
    stmt.treeName == "Apply" && {
      // Arguments to assert/require (strictly, should ignore only the 2nd argument)
      stmt.symbolName == "scala.Predef.assert" ||
      stmt.symbolName == "scala.Predef.require"
    }
  } groupBy {
    _.location.fullClassName
  } mapValues {
    // Note start + 1 to ensure range does not include this stmt itself
    _.map(stmt => Range(stmt.start + 1, stmt.end)).toSet
  }

  // Predicate for ignored statements
  def isIgnored(stmt: Statement): Boolean = {
    def isAssertArgument(stmt: Statement): Boolean =
      ignoreRanges.get(stmt.location.fullClassName) match {
        case None => false
        case Some(set) =>
          set exists { range => (range contains stmt.start) && (range contains stmt.end) }
      }
    stmt.symbolName == "com.argondesign.alogic.util.unreachable" ||
    stmt.symbolName == "com.argondesign.alogic.core.Messaging.ice" ||
    stmt.desc.startsWith("throw com.argondesign.alogic.core.Messages.Ice") ||
    isAssertArgument(stmt)
  }

  // Compute updated coverage by marking ignored statements as such
  val updatedCoverage = Coverage()
  coverage.statements.iterator map {
    case stmt if isIgnored(stmt) => stmt.copy(ignored = true)
    case stmt                    => stmt
  } foreach updatedCoverage.add

  // Save updated coverage
  Serializer.serialize(updatedCoverage, coverageFile)
}

// UpdateIgnored before Report
coverageReport := (coverageReport dependsOn coverageUpdateIgnored).value
