
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

  val listOfFiles: List[Path] = conf.path() match {
    case IsFile(file)     => List(file)
    case IsDirectory(dir) => dir.descendants("*.alogic", 1).toList
  }

  val odir = conf.odir()
  if (!odir.exists) {
    odir.createDirectory()
  }

  val multiThreaded = conf.parallel()

  Message.verbose = conf.verbose()

  val includeSearchPaths = conf.incdir()

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
    Message.info(s"Waiting for ${conf.path().path} to be modified (press return to quit)...")
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get conf.path().path,
      callback = { _ => go })
    io.StdIn.readLine()
    Message.info("Quitting")
    system.terminate()
  }

  sys exit (if (Message.fail) 1 else 0)

  def go() {
    val t0 = System.nanoTime()

    portMap.clear()

    // Construct potentially parallel file list
    val filePaths = if (multiThreaded) listOfFiles.par else listOfFiles

    // First pass
    val asts = filePaths map { path: Path =>
      val parser = new AParser(includeSearchPaths)

      // Build AST
      Message.info(s"Parsing ${path.path}")
      val ast = parser(path)

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

    Message.info(s"Compilation time: ${(System.nanoTime() - t0) / 1e9}s")
  }
}
