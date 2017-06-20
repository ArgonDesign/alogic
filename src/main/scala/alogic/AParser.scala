package alogic

import org.antlr.v4.runtime.ANTLRInputStream
import org.antlr.v4.runtime.CommonTokenStream

import scalax.file.Path
import alogic.ast.AstBuilder

object AParser {

  def apply(path: Path, includeSearchPaths: List[Path] = Nil, initalDefines: Map[String, String] = Map()): List[ast.Task] = {

    Message.info(s"Parsing ${path.path}")

    // First preprocess input file to deal with #define s
    val preprocessed = Preproc(path, includeSearchPaths, initalDefines)

    // Now parse the file, return None if syntax error
    val parseTree = {
      val inputStream = new ANTLRInputStream(preprocessed)
      inputStream.name = path.path
      val lexer = new antlr.VLexer(inputStream)
      val tokenStream = new CommonTokenStream(lexer)
      tokenStream.fill()

      val parser = new antlr.VParser(tokenStream)
      parser.removeErrorListeners()
      parser.addErrorListener(ParserErrorListener)
      val paseTree = parser.start()
      if (parser.getNumberOfSyntaxErrors == 0)
        Some(paseTree)
      else
        None
    }

    // If Some, build the AST
    parseTree match {
      case None    => Nil
      case Some(t) => AstBuilder(t)
    }
  }

}
