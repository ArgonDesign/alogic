package alogic

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

import scalax.file.Path

class AParser(includeSearchPaths: List[Path]) {

  val builder = new AstBuilder()

  def apply(path: Path): Program = {
    // First preprocess input file to deal with #define s
    val preproc = new Preproc(includeSearchPaths)
    val preprocessed: String = preproc(path)

    // Now parse the file
    val inputStream = new ANTLRInputStream(preprocessed)
    inputStream.name = path.path
    val lexer = new antlr.VLexer(inputStream)
    val tokenStream = new CommonTokenStream(lexer)
    tokenStream.fill()

    val parser = new antlr.VParser(tokenStream)
    parser.removeErrorListeners()
    parser.addErrorListener(ParserErrorListener)
    val parseTree = parser.start()
    val ast = builder(parseTree)
    val errCount = parser.getNumberOfSyntaxErrors()
    if (errCount > 0) {
      Message.error(s"Syntax error count is $errCount")
    }
    ast // TODO if have parsing errors should not continue compilation - return Option instead?
  }

}
