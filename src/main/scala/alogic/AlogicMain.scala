
package alogic
import java.io.File

import akka.actor.ActorSystem
import com.beachape.filemanagement.MonitorActor
import com.beachape.filemanagement.RegistryTypes._
import com.beachape.filemanagement.Messages._

import java.io.{FileWriter, BufferedWriter}

import java.nio.file.Paths
import java.nio.file.StandardWatchEventKinds._

// Run 1000 times, Lex of all files takes 15s single threaded, or 5 seconds multi threaded

object AlogicMain extends App {

  val multiThreaded = false;

  def getListOfFiles(dir: File): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { s => s.getName.endsWith("alogic") }
  }
  
  val useMonitor = args.length==2 && args(0)=="-m"
      
  if ((args.length==2 && !useMonitor) || args.length>2 || args.length<1) {
      println("Syntax: alogic [-m] (source_file|source_dir)")
      println("-m tells alogic to recompile whenever the source changes.")
      System.exit(-1)
  }
  go
  if (useMonitor) {
    implicit val system = ActorSystem("actorSystem")
    val fileMonitorActor = system.actorOf(MonitorActor(concurrency = 2))
    println(s"Waiting for ${args(1)} to be modified (press return to quit)...") 
    fileMonitorActor ! RegisterCallback(
      event = ENTRY_MODIFY,
      path = Paths get args(1),
      callback =  {_=>go}
      )
    io.StdIn.readLine()
    println("Quitting")
    system.terminate()
  } 
  
  def go() {
    val codeFile = args.last
    val d = new File(codeFile)
    val t0 = System.nanoTime()
    if (d.exists && d.isDirectory) {
        val lst = getListOfFiles(d)
        if (multiThreaded) {
          // Start threads
          val threads = for {f <- lst} yield {
            // make copies of the Lexer and Parser to avoid conflicts
            val thread = new Thread {
              override def run {
                  AParser(f.getPath)
              }
            }
            thread.start
            thread
          }
          // Join threads
          for {t <- threads} t.join()
        } else {
          for {f <- lst} AParser(f.getPath)
        }
    } else {
        val s = AParser(codeFile)
        println(s.parseTree)
    }
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000.0 + "s")
    
  }
}
