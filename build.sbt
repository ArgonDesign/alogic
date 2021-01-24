////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
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

crossScalaVersions := Seq(scalaVersion.value, "3.0.0-M3")

scalacOptions ++= Seq("-feature", "-unchecked")

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, n)) => Seq("-explain", "-explain-types", "-source:3.0")
    case _            => Seq("-explaintypes", "-Xlint:_")
  }
}

// Note: Needed for Scala 3 Dokka dependencies, which are planned to be
// removed at some point. Remove when that happens
ThisBuild / resolvers += Resolver.JCenterRepository

////////////////////////////////////////////////////////////////////////////////
// Some of the build is conditional and built only on Java 11
////////////////////////////////////////////////////////////////////////////////

val onJava11 = System.getProperty("java.version").startsWith("11.")

unmanagedSources / excludeFilter := {
  if (onJava11) {
    ""
  } else {
    new SimpleFileFilter(_.getCanonicalPath contains "com/argondesign/alogic/gcp")
  }
}

////////////////////////////////////////////////////////////////////////////////
// Library dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.rogach" %% "scallop" % "4.0.1"

libraryDependencies += {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, n)) => "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0"
    case _            => "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
  }
}

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
) map {
  _ % "0.14.0-M3"
}

// Java 11 only
libraryDependencies ++= {
  if (onJava11) {
    Seq("com.google.cloud.functions" % "functions-framework-api" % "1.0.3")
  } else {
    Seq.empty
  }
}

////////////////////////////////////////////////////////////////////////////////
// Testing dependencies
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test

libraryDependencies += "org.mockito" % "mockito-core" % "3.7.7" % Test

logBuffered in Test := false

testOptions in Test += Tests.Argument("-oD") // Add F for full stack traces

////////////////////////////////////////////////////////////////////////////////
// Target aliases
////////////////////////////////////////////////////////////////////////////////

addCommandAlias(
  "runUnitTests",
  """testOnly -- -l "com.argondesign.alogic.tags.EndToEndTest""""
)

addCommandAlias(
  "runEndToEndTests",
  """testOnly -- -n "com.argondesign.alogic.tags.EndToEndTest""""
)

////////////////////////////////////////////////////////////////////////////////
// Antlr4 plugin
////////////////////////////////////////////////////////////////////////////////

enablePlugins(Antlr4Plugin)

antlr4Version in Antlr4 := "4.9.0"

antlr4Dependency in Antlr4 := "com.tunnelvisionlabs" % "antlr4" % (Antlr4/antlr4Version).value

antlr4RuntimeDependency in Antlr4 := "com.tunnelvisionlabs" % "antlr4-runtime" % (Antlr4/antlr4Version).value

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
  coverageEnabled,
  BuildInfoKey.action("buildTime")(System.currentTimeMillis)
)

buildInfoPackage := "com.argondesign.alogic"

////////////////////////////////////////////////////////////////////////////////
// SBT scoverage
////////////////////////////////////////////////////////////////////////////////

coverageOutputXML := true
coverageOutputHTML := !(sys.env.get("GITHUB_ACTIONS") contains "true")
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

////////////////////////////////////////////////////////////////////////////////
// SBT assembly
////////////////////////////////////////////////////////////////////////////////

// Do not run tests when running assembly
test in assembly := {}

// Put output in it's own directory as needed by GCP
assemblyOutputPath in assembly := crossTarget.value / "assembly" / (assemblyJarName in assembly).value

////////////////////////////////////////////////////////////////////////////////
// Google Cloud Functions
////////////////////////////////////////////////////////////////////////////////

val gcfRun = taskKey[Unit]("Run local server for Google Cloud Function endpoint")
gcfRun := {
  import scala.collection.mutable
  import com.google.cloud.functions.invoker.runner.Invoker

  val log = streams.value.log
  val classPath = (Runtime / fullClasspath).value map { _.data } mkString ":"

  val args = new mutable.ArrayBuffer[String]
  args.append("--classpath")
  args.append(classPath)
  args.append("--target")
  args.append("com.argondesign.alogic.gcp.FunctionCompile")
  log.info("Calling Invoker with " + args);
  Invoker.main(args.toArray)
}

val gcfDeploy = taskKey[Unit]("Deploy Google Cloud Functions to GCP")
gcfDeploy := {
  import scala.sys.process._

  val log = streams.value.log

  Seq(
    "gcloud",
    "--project=ccx-eng-cam",
    "functions",
    "deploy",
    "alogic-playground",
    "--region=us-central1",
    "--service-account=alogic-playground@ccx-eng-cam.iam.gserviceaccount.com",
    "--entry-point=com.argondesign.alogic.gcp.FunctionCompile",
    "--runtime=java11",
    "--memory=512MB",
    "--timeout=80s",
    "--trigger-http",
    "--allow-unauthenticated",
    s"--source=${assembly.value.getParent}"
  ) ! ProcessLogger { s: String => log.info(s) } ensuring { _ == 0 }
}
