////////////////////////////////////////////////////////////////////////////////
// General
////////////////////////////////////////////////////////////////////////////////

name := "alogic_antlr"

organization := "alogic"

version := "1"

scalaVersion := "2.12.1"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

////////////////////////////////////////////////////////////////////////////////
// Antlr4 plugin
////////////////////////////////////////////////////////////////////////////////

antlr4Settings

antlr4PackageName in Antlr4 := Some("alogic.antlr4")

antlr4GenListener in Antlr4 := true

antlr4GenVisitor in Antlr4 := true

////////////////////////////////////////////////////////////////////////////////
// Antlr4 postprocessing
////////////////////////////////////////////////////////////////////////////////

val antlr4PostprocessDir = SettingKey[File]("Directory to place Antlr4 postprocessor output")
antlr4PostprocessDir := (sourceManaged in Compile).value / "antlr4Post"
managedSourceDirectories in Compile <+= antlr4PostprocessDir
cleanFiles <+= antlr4PostprocessDir

val antlr4Postprocess = TaskKey[Seq[File]]("Derive further sources from Antlr4 output")
antlr4Postprocess := {
  val parsers = (antlr4Generate in Antlr4).value.filter(_.name matches ".*Parser.java")
  val pkg = (antlr4PackageName in Antlr4).value.getOrElse("")
  val pkgDir = pkg.replaceAll("\\.", "/")

  var ruleContextsTraits: List[sbt.File] = Nil
  for (parser <- parsers) {
    val parserName = parser.name.takeWhile(_ != '.')
    val ruleContextsTrait = antlr4PostprocessDir.value / pkgDir / s"${parserName}RuleContexts.scala"
    val ruleContextsTraitText = new StringBuilder()

    ruleContextsTraitText ++= (pkg match {
      case "" => ""
      case _ => s"package $pkg\n\n"
    })

    ruleContextsTraitText ++= s"trait ${parserName}RuleContexts {\n"

    for (line <- scala.io.Source.fromFile(parser).getLines()) {
      val pattern = """.*public static class (.*) extends \w+Context .*""".r
      line match {
        case pattern(name) => ruleContextsTraitText ++= s"  type $name = $parserName.$name\n"
        case _ => ;
      }
    }

    ruleContextsTraitText ++= "}\n"

    IO.write(ruleContextsTrait, ruleContextsTraitText.toString)
    ruleContextsTraits = ruleContextsTrait :: ruleContextsTraits
  }
  ruleContextsTraits
}
sourceGenerators in Compile <+= antlr4Postprocess

////////////////////////////////////////////////////////////////////////////////
// ScalaTest
////////////////////////////////////////////////////////////////////////////////

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

logBuffered in Test := false
