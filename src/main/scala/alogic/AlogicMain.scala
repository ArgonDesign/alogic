
package alogic

import java.io.File
import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._

import scala.collection.concurrent.TrieMap

import com.beachape.filemanagement.Messages._
import com.beachape.filemanagement.MonitorActor

import AstOps._
import akka.actor.ActorSystem
import scalax.file.PathMatcher._

object AlogicMain extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val conf = new CLIConf(args)

  val listOfHeaders: List[File] = conf.headers() map (_.fileOption.get)

  val listOfFiles: List[File] = conf.ipath() match {
    case IsFile(file)     => List(file.fileOption.get)
    case IsDirectory(dir) => dir.descendants("*.alogic", 1).toList map (_.fileOption.get)
  }

  val outputdir = conf.odir().path
  val outdir = new File(outputdir)
  if (!outdir.exists())
    outdir.mkdir()

  val multiThreaded = conf.parallel();

  //////////////////////////////////////////////////////////////////////////////
  // Compile
  //////////////////////////////////////////////////////////////////////////////

  ///println("""python test.py""".!)  Example of how to run python code
  // .!! gets output
  // Process("").lines runs in background and provides a Stream[String] of output
  // Throws exception if non-zero exit code

  val portMap = new TrieMap[String, Task]() // This must be from a concurrent collection because it is populated from multiple threads

  go
  go

  if (conf.monitor()) {
    implicit val system = ActorSystem("actorSystem")
    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
    println(s"Waiting for ${conf.ipath().path} to be modified (press return to quit)...")
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get conf.ipath().path,
      callback = { _ => go })
    io.StdIn.readLine()
    println("Quitting")
    system.terminate()
  }

  def go() {
    val t0 = System.nanoTime()

    portMap.clear()

    // Parse header files
    val parser = new AParser()
    for (f <- listOfHeaders) {
      parser(f.getPath)
    }

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
    val fileList = listOfFiles map (_.getPath)

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
