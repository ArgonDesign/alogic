////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Language server implementation
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lsp

import com.argondesign.alogic.Compiler
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages._
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.frontend.Frontend
import com.google.gson.JsonObject
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._

import java.io.File
import java.io.PrintWriter
import java.util.concurrent.CompletableFuture
import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters._
import scala.jdk.FutureConverters._
import scala.util.Failure
import scala.util.Success

class AlogicLanguageServer extends LanguageServer with LanguageClientAware {

  private var client: LanguageClient = null

  var workspaceFolders: Seq[WorkspaceFolder] = Nil
  var extraCommandLineOpts: Seq[String] = Nil

  def initialize(x: InitializeParams): CompletableFuture[InitializeResult] = {
    workspaceFolders = x.getWorkspaceFolders().asScala.toList
    val capabilities = new ServerCapabilities();
    capabilities.setTextDocumentSync(TextDocumentSyncKind.Full);
    val semTokenLegend = new SemanticTokensLegend(
      SemanticTokenType.values.toList.map(_.toString).asJava,
      SemanticTokenModifier.values.toList.map(_.toString).asJava
    )
    capabilities.setSemanticTokensProvider(
      new SemanticTokensWithRegistrationOptions(
        semTokenLegend,
        new SemanticTokensServerFull(false),
        false,
        List(new DocumentFilter("alogic", "", "")).asJava
      )
    )
    CompletableFuture.completedFuture(new InitializeResult(capabilities))
  }

  def shutdown(): CompletableFuture[Object] = {
    CompletableFuture.completedFuture(null)
  }

  def exit() = {}

  val fullTextDocumentService = new FullTextDocumentService() {

    override def didChange(params: DidChangeTextDocumentParams) = {
      super.didChange(params);
      validateDocument(documents(params.getTextDocument().getUri()))
    }

    override def didOpen(params: DidOpenTextDocumentParams) = {
      super.didOpen(params)
      validateDocument(documents(params.getTextDocument().getUri()))
    }

  }

  def getTextDocumentService(): TextDocumentService = {
    fullTextDocumentService
  }

  def getWorkspaceService(): WorkspaceService = {
    new WorkspaceService() {
      override def symbol(params: WorkspaceSymbolParams) = {
        null;
      }

      def didChangeConfiguration(params: DidChangeConfigurationParams) = {
        val gloabalSettings = params.getSettings.asInstanceOf[JsonObject]
        extraCommandLineOpts = gloabalSettings
          .getAsJsonObject("alogic-lang")
          .getAsJsonArray("extraCommandLineOpts")
          .asScala
          .map(_.getAsString)
          .toSeq
        client.workspaceFolders.asScala andThen {
          case Success(folders) => {
            val perWSOpts = gloabalSettings
              .getAsJsonObject("alogic-lang")
              .getAsJsonArray("perWorkspaceCmdOpts")
              .asScala
              .map(_.getAsString)
              .toSeq
            val prefix = "file://"
            extraCommandLineOpts =
              extraCommandLineOpts ++ folders.asScala.filter(_.getUri.startsWith(prefix)).flatMap {
                folder =>
                  perWSOpts map {
                    _.replaceAll("\\$\\{workspaceFolder}", folder.getUri.substring(prefix.length))
                  }
              }
            fullTextDocumentService.documents.values.foreach(validateDocument)
          }
          case Failure(e) => e.printStackTrace
        }
      }

      def didChangeWatchedFiles(params: DidChangeWatchedFilesParams) = {}
    }
  }

  override def connect(client: LanguageClient) = {
    this.client = client
  }

  private def validateDocument(document: TextDocumentItem) = {
    val prefix = "file://"
    var tempFile: File = null
    val path = if (document.getUri.startsWith(prefix)) {
      document.getUri.substring(prefix.length)
    } else {
      tempFile = File.createTempFile("alogic-lang-", ".alogic")
      new PrintWriter(tempFile) {
        try {
          write(document.getText)
        } finally {
          close()
        }
      }
      tempFile.getAbsolutePath
    }

    val source = Source(path, document.getText)

    val mb = new MessageBuffer

    Compiler.parseArgs(
      mb,
      extraCommandLineOpts :++ List("-o", System.getProperty("java.io.tmpdir"), path),
      None
    ) match {
      case Some((settings, _, params)) => {

        implicit val cc = new CompilerContext(mb, settings)
        val fe = new Frontend
        fe(source, Loc(document.getUri, 1, source, 0, 0, 0), Nil)

        val sources = mb.messages.map(_.loc.source.path).toSet
        sources.foreach(src => {
          val uri = if (src == path) document.getUri else "file://" + src
          client.publishDiagnostics(
            new PublishDiagnosticsParams(
              uri,
              mb.messages
                .filter(_.loc.source.path == src)
                .map(msg => {
                  val startLineOffset =
                    msg.loc.source.offsetFor(msg.loc.source.lineFor(msg.loc.start))
                  new Diagnostic(
                    new Range(
                      new Position(msg.loc.line - 1, msg.loc.start - startLineOffset),
                      new Position(msg.loc.line - 1, msg.loc.end - startLineOffset)
                    ),
                    msg.msg.mkString("\n"),
                    msg.category match {
                      case WarningCategory => DiagnosticSeverity.Warning
                      case ErrorCategory   => DiagnosticSeverity.Error
                      case NoteCategory    => DiagnosticSeverity.Information
                      case FatalCategory   => DiagnosticSeverity.Error
                      case IceCategory     => DiagnosticSeverity.Error
                    },
                    "alogic-lang"
                  )
                })
                .asJava
            )
          )
        })
        // Push empty diagnostics for current file if not in sources
        if (!sources.contains(path)) {
          client.publishDiagnostics(new PublishDiagnosticsParams(document.getUri, Nil.asJava))
        }

      }
      case None => {
        client.showMessage(new MessageParams(MessageType.Error, "Command line parsing failed"))
        println((extraCommandLineOpts :+ path).mkString("\n"))
        mb.messages.foreach(msg => println(msg.msg.mkString("\n")))
      }
    }
    if (tempFile != null) {
      tempFile.delete
    }
  }

}
