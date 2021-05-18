////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// TextDocumentService implementation which doesn't handle deltas
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lsp

import com.argondesign.alogic.antlr.AlogicLexer
import com.argondesign.alogic.antlr.AlogicParser
import com.argondesign.alogic.antlr.AlogicTokenFactory
import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Source
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services.TextDocumentService

import java.util.concurrent.CompletableFuture
import scala.jdk.CollectionConverters._
import scala.util.chaining._

class FullTextDocumentService extends TextDocumentService {

  var documents: scala.collection.mutable.Map[String, TextDocumentItem] =
    scala.collection.mutable.Map()

  override def didOpen(params: DidOpenTextDocumentParams) = {
    documents += (params.getTextDocument().getUri() -> params.getTextDocument())
  }

  override def didChange(params: DidChangeTextDocumentParams) = {
    val uri = params.getTextDocument().getUri()
    params.getContentChanges().forEach(change => documents(uri).setText(change.getText()));
  }

  override def didClose(params: DidCloseTextDocumentParams) = {
    documents -= params.getTextDocument().getUri()
  }

  override def didSave(params: DidSaveTextDocumentParams) = {}

  override def semanticTokensFull(
      params: SemanticTokensParams
    ): CompletableFuture[SemanticTokens] = {
    val document = documents(params.getTextDocument().getUri())

    // Create Antlr4 parser
    val prefix = "file:///"
    val path = if (document.getUri().startsWith(prefix)) {
      document.getUri().substring(prefix.length())
    } else {
      document.getUri()
    }

    val source = Source(path, document.getText())

    val tokens = {
      val mb = new MessageBuffer
      val tokenFactory = new AlogicTokenFactory(source, mb)
      val stream = CharStreams.fromString(document.getText())
      val lexer = new AlogicLexer(stream)
      lexer.setTokenFactory(tokenFactory)
      val tokenStream = new CommonTokenStream(lexer)
      val parser = new AlogicParser(tokenStream) tap { parser =>
        parser.removeErrorListeners()
      }
      val visitor = new SemanticTokenVisitor()
      val parseResult = parser.file // Needs to be called for tokenStream to be filled
      val semTokens = visitor(parseResult)
      (semTokens ++ tokenStream.getTokens.asScala
        .filter(_.getChannel == AlogicLexer.COMMENT)
        .map(tk => SemanticToken(tk.loc, SemanticTokenType.Comment))).sortBy(_.loc.start)
    }

    val lineOffsets = source.lines.foldLeft(Array(0))((a, l) => a :+ a.last + l.length)
    val lineLens = source.lines.map(_.length()).toArray

    // Binary search to find correct line for given offset
    def getLineForOffset(offset: Int): Int = {
      def search(start: Int = 0, end: Int = lineOffsets.length - 1): Int = {
        val mid = start + (end - start) / 2
        if (
          offset >= lineOffsets(end) && (end == lineOffsets.length - 1 || offset < lineOffsets(
            end + 1
          ))
        ) end
        else if (offset >= lineOffsets(start) && offset < lineOffsets(start + 1)) start
        else if (offset >= lineOffsets(mid) && offset < lineOffsets(mid + 1)) mid
        else if (offset >= lineOffsets(mid)) search(mid + 1, end)
        else search(start, mid - 1)
      }
      search()
    }

    // Split loc covering multiple lines into 1 per line
    def splitLoc(loc: Loc): Seq[Loc] = {
      val startLine = getLineForOffset(loc.start)
      LazyList.from(0) takeWhile (line => {
        lineOffsets(startLine + line) <= loc.end && startLine + line < lineLens.size
      }) map { line =>
        val realLine = startLine + line
        val startPos = loc.start.max(lineOffsets(realLine))
        val endPos = loc.end.min(lineOffsets(realLine) + lineLens(realLine))
        Loc(loc.file, loc.line + line, loc.source, startPos, endPos, startPos)
      }
    }

    // Generate list of 5 integers per semantic token as per:
    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
    // [line (zero indexed), start char on line, length, type, modifiers]
    val tkData = tokens filter (_.loc.start >= 0) flatMap { tk =>
      splitLoc(tk.loc) map { loc =>
        List[Integer](
          loc.line - 1,
          loc.start - lineOffsets(getLineForOffset(loc.start)),
          loc.end - loc.start,
          tk.typ.id,
          tk.getEncodedModifiers()
        )
      }
    }

    // Delta encode the line and start character
    val deltaTkData = tkData match {
      case head :: next =>
        head ++ tkData.zip(next).flatMap {
          case (prev, cur) => {
            List[Integer](
              cur(0) - prev(0),
              if (cur(0) == prev(0)) cur(1) - prev(1)
              else cur(1),
              cur(2),
              cur(3),
              cur(4)
            )
          }
        }
      case Nil => Nil
    }

    CompletableFuture.completedFuture(new SemanticTokens(deltaTkData.asJava))
  }

}
