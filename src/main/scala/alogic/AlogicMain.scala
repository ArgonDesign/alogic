
package alogic

import AstOps._
import java.io.File
import sys.process._

import akka.actor.ActorSystem
import com.beachape.filemanagement.MonitorActor
import com.beachape.filemanagement.RegistryTypes._
import com.beachape.filemanagement.Messages._

import java.io.{ FileWriter, BufferedWriter }

import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._
import scala.collection.concurrent.TrieMap

object AlogicMain extends App {

  val portMap = new TrieMap[String, Task]() // This must be from a concurrent collection because it is populated from multiple threads

  val multiThreaded = false; // At the moment there does not seem to be much benefit from multithreading, so leave it off in order that error messages are in correct order

  def getListOfFiles(dir: File): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { s => s.getName.endsWith("alogic") }
  }

  val useMonitor = args.length > 1 && args(0) == "-m"
  val args2 = if (useMonitor) args.tail else args

  ///println("""python test.py""".!)  Example of how to run python code
  // .!! gets output
  // Process("").lines runs in background and provides a Stream[String] of output
  // Throws exception if non-zero exit code

  if (args2.length > 2 || args2.length < 1) {
    println("Syntax: alogic [-m] [header file]* (source_file|source_dir)")
    println("-m tells alogic to recompile whenever the source changes.")
    System.exit(-1)
  }

  val outputdir = "generated"
  val outdir = new File(outputdir)
  if (!outdir.exists())
    outdir.mkdir()
  go
  go
  if (useMonitor) {
    implicit val system = ActorSystem("actorSystem")
    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
    println(s"Waiting for ${args(1)} to be modified (press return to quit)...")
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get args(1),
      callback = { _ => go })
    io.StdIn.readLine()
    println("Quitting")
    system.terminate()
  }

  def go() {
    val t0 = System.nanoTime()
    val codeFile = args.last

    portMap.clear()

    // Parse header files
    val parser = new AParser()
    for (f <- args2.init) parser(f)

    // This method is called for each file that should be converted to Verilog
    def compileFile(fname: String): (String, Program) = {
      val parser2 = new AParser(parser) // Capture all structures from header files

      // Build AST
      val ast = parser2(fname)

      // Extract ports
      VisitAST(ast) {
        case t @ Task(_, name, _, _) => {
          if (portMap contains name)
            println(s"WARNING: $name defined multiple times")
          portMap(name) = t; false
        }
        case _ => true // Recurse
      }
      return (fname, ast)
    }

    def buildFile(params: (String, Program)): Unit = {
      val (fname, ast) = params
      // Convert to state machine
      val prog: StateProgram = new MakeStates()(ast)

      // Remove complicated assignments and ++ and -- (MakeStates inserts some ++/--)
      val prog2 = Desugar.RemoveAssigns(prog)

      // Construct output filename
      val f0 = new File(fname).getName
      val f = new File(outdir, f0 + ".v").getPath()

      // Write Verilog
      new MakeVerilog()(prog2, f)
    }

    // Construct file list
    val d = new File(codeFile)
    val fileList = if (d.exists && d.isDirectory)
      for (f <- getListOfFiles(d)) yield f.getPath
    else
      codeFile :: Nil

    // First pass
    val asts = if (multiThreaded)
      fileList.par.map(compileFile).seq.toList
    else
      fileList.map(compileFile)

    // Second pass
    if (multiThreaded)
      asts.par.foreach(buildFile)
    else
      asts.foreach(buildFile)

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + "s")

  }
}
