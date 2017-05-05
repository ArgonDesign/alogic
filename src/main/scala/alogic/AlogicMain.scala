
package alogic
import java.io.File

// Run 1000 times, Lex of all files takes 15s single threaded, or 5 seconds multi threaded

object AlogicMain extends App {

  val multiThreaded = true;

  def getListOfFiles(dir: File): List[File] = {
    dir.listFiles.filter(_.isFile).toList.filter { s => s.getName.endsWith("alogic") }
  }

  if (args.length!=1) {
      println("Syntax: alogic (source_file|source_dir)")
      System.exit(-1)
  }
  val codeFile = args.last
  val d = new File(codeFile)
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
}
