package alogic

import org.antlr.v4.runtime.{ ANTLRInputStream, CommonTokenStream }

// TODO figure out how to do preprocessor
//   A) Put into VParser.g4
//       Problem is that there are many places where a #if could appear, controlling ports/modules/statements and the grammar seems to explode
//   B) Put into VLexer.g4 using actions
//     This feels like the right place, but implementation is unclear.
//       Problem is that it is not clear whether we can use the same trick of parsing header file once, and then reusing the defines
//       Perhaps can make lexer once and give it more input?
//       Perhaps can add method to adjust the defines dictionary?

import scala.io.Source
import java.io.File
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer
import org.antlr.v4.runtime.BaseErrorListener

object ParserErrorListener extends BaseErrorListener {
  override def syntaxError(recognizer: Recognizer[_, _],
                           offendingSymbol: Object,
                           line: Int,
                           charPositionInLine: Int,
                           msg: String,
                           e: RecognitionException) = {
    val loc = Loc(recognizer.getInputStream.getSourceName, line, charPositionInLine)
    println(loc + " Syntax error: " + msg)
  }
}

class AParser() {

  val builder = new AstBuilder()
  val preproc = new Preproc()

  def this(old: AParser) {
    this()
    builder.add(old.builder)
    preproc.add(old.preproc)
  }

  def loadFile(filename: String): String = {
    println(filename)
    val bufferedSource = Source.fromFile(filename)
    val code = bufferedSource.mkString
    bufferedSource.close
    code
  }

  def apply(path: String): Program = {

    val pinputStream = new ANTLRInputStream(loadFile(path))
    pinputStream.name = path

    // First preprocess input file to deal with #define s
    val plexer = new antlr.VPreprocLexer(pinputStream)
    val ptokenStream = new CommonTokenStream(plexer)
    ptokenStream.fill()

    val pparser = new antlr.VPreprocParser(ptokenStream)
    pparser.removeErrorListeners()
    pparser.addErrorListener(ParserErrorListener)
    val pparseTree = pparser.start()
    val preprocessed: String = preproc(pparseTree)

    // Now parse the file
    val inputStream = new ANTLRInputStream(preprocessed)
    inputStream.name = path
    val lexer = new antlr.VLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new antlr.VParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(ParserErrorListener)
    val parseTree = parser.start()
    val ast = builder(parseTree)
    //println(ast)
    val errCount = parser.getNumberOfSyntaxErrors()
    if (errCount > 0) {
      println(s"Parsing error count is $errCount in $path")
    }
    ast // TODO if have parsing errors should not continue compilation - return Option instead?
  }

}
