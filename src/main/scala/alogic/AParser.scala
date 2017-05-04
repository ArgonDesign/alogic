package alogic

import org.antlr.v4.runtime.{ ANTLRInputStream, CommonTokenStream }

import scala.io.Source
import java.io.File

class AParser(text: String) {

  def loadFile(filename:String):String = {
    val bufferedSource = Source.fromFile(filename)
    val code = bufferedSource.mkString
    bufferedSource.close
    code
  }

  val tokenStream = {
    val inputStream = new ANTLRInputStream("abc def /* test */ ghi 6")
    inputStream.name = text
    val lexer = new antlr4.VLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()
    tokenStream
  }

  val parseTree = {
    val parser = new antlr4.VParser(tokenStream)
    parser.start()
  }

}

object AParser {
  def apply(path: String) = new AParser(path)
}
