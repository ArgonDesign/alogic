package alogic

import org.antlr.v4.runtime.{ ANTLRInputStream, CommonTokenStream }

import scala.io.Source
import java.io.File

class AParser() {

  val builder = new AstBuilder()

  def this(old: AParser) {
    this()
    builder.add(old.builder)
  }

  def loadFile(filename: String): String = {
    println(filename)
    val bufferedSource = Source.fromFile(filename)
    val code = bufferedSource.mkString
    bufferedSource.close
    code
  }

  object myVisitor extends antlr.VParserBaseVisitor[String] {
    override def visitBinaryExpr(ctx: antlr.VParser.BinaryExprContext) = {
      //println(ctx.binary_op) TODO why does this print out a long list of stuff?
      visitChildren(ctx)
      "test"
    }
  }

  def apply(path: String): AlogicAST = {

    val inputStream = new ANTLRInputStream(loadFile(path))
    inputStream.name = path + '\n' // TODO why have error messages stopped reporting the input stream?
    val lexer = new antlr.VLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new antlr.VParser(tokenStream)
    val parseTree = parser.start()
    val ast = builder(parseTree)
    //println(ast)
    val errCount = parser.getNumberOfSyntaxErrors()
    if (errCount > 0) {
      println(s"Parsing error count is $errCount in $path")
    }
    ast
  }

}

