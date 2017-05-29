
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
import scalax.file.Path

object AlogicMain extends App {

  //////////////////////////////////////////////////////////////////////////////
  // Parse arguments
  //////////////////////////////////////////////////////////////////////////////

  val conf = new CLIConf(args)

  val listOfHeaders: List[Path] = conf.headers()

  val listOfFiles: List[Path] = conf.ipath() match {
    case IsFile(file)     => List(file)
    case IsDirectory(dir) => dir.descendants("*.alogic", 1).toList
  }

  val odir = conf.odir()
  if (!odir.exists) {
    odir.createDirectory()
  }

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

  sys exit (if (Message.fail) 1 else 0)

  def go() {
    val t0 = System.nanoTime()

    portMap.clear()

    // Parse header files
    val parser = new AParser()
    for (f <- listOfHeaders) {
      parser(f.path)
    }

    // Construct potentially parallel file list
    val filePaths = if (multiThreaded) listOfFiles.par else listOfFiles

    // First pass
    val asts = filePaths map { path: Path =>
      val parser2 = new AParser(parser) // Capture all structures from header files

      // Build AST
      val ast = parser2(path.path)

      // Extract ports
      VisitAST(ast) {
        case t @ Task(_, name, _, _) => {
          if (portMap contains name)
            Message.warning(s"$name defined multiple times")
          portMap(name) = t; false
        }
        case _ => true // Recurse
      }

      (path, ast)
    }

    // Second pass
    asts.foreach {
      case (path: Path, ast: Program) =>
        // Convert to state machine
        val prog: StateProgram = new MakeStates()(ast)

        // Remove complicated assignments and ++ and -- (MakeStates inserts some ++/--)
        val prog2 = Desugar.RemoveAssigns(prog)

        // Construct output filename
        val opath: Path = odir / (path.name + ".v")

        // Write Verilog
        new MakeVerilog()(prog2, opath.path)
    }

    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1000000000.0 + "s")
  }
}
