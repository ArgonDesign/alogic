package alogic

import org.antlr.v4.runtime.{ ANTLRInputStream, CommonTokenStream }

import scala.io.Source
import java.io.File

class AParser(path: String) {

  def loadFile(filename:String):String = {
    val bufferedSource = Source.fromFile(filename)
    val code = bufferedSource.mkString
    bufferedSource.close
    code
  }
  
  object myVisitor extends antlr4.VParserBaseVisitor[String] {
    override def visitSourceText(ctx: antlr4.VParser.SourceTextContext) = {
      println(ctx.IDENTIFIER)
      visitChildren(ctx)
      "test"
    }
  }

  val tokenStream = { 
    val inputStream = new ANTLRInputStream(loadFile(path))
    inputStream.name = path
    val lexer = new antlr4.VLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()
    tokenStream
  }
  
  val parseTree = {
    val parser = new antlr4.VParser(tokenStream)
    val parseTree = parser.start()
    myVisitor.visit(parseTree)
    parseTree
  }

}

object AParser {
  def apply(path: String) = new AParser(path)
}
