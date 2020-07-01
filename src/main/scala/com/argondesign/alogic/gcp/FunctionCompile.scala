////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Google Cloud Function compiler endpoint
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.gcp

import com.argondesign.alogic.BuildInfo
import com.argondesign.alogic.Compiler
import com.argondesign.alogic.core.MessageBuffer
import com.argondesign.alogic.core.Messages
import com.argondesign.alogic.core.Source
import com.argondesign.alogic.CommandLineInterface
import com.argondesign.alogic.core.Loc
import com.google.cloud.functions.HttpFunction
import com.google.cloud.functions.HttpRequest
import com.google.cloud.functions.HttpResponse
import io.circe.Json
import io.circe.JsonObject
import io.circe.generic.auto._
import io.circe.syntax._

import java.io.File
import java.io.PrintWriter
import java.io.StringWriter
import java.net.HttpURLConnection
import java.nio.file.Files
import java.nio.file.Path
import java.util.logging.Logger
import java.util.stream.Collectors
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

class FunctionCompile extends HttpFunction {

  // API version reported to client
  private val apiVersion: Int = 1

  // Logger
  private val logger = Logger.getLogger(getClass.getName)

  // Placeholder in case no Origin header is present in HTTP request
  private val noOrigin = java.util.Arrays.asList("<no-origin>")

  override def service(request: HttpRequest, response: HttpResponse): Unit = {

    // Log a warning if invoked from outside playground
    request.getHeaders
      .getOrDefault("Origin", noOrigin)
      .forEach { origin =>
        if (origin != "https://alogic.app") {
          logger.warning("Request from outside playground: " + origin)
        }
      }

    def replyBadRequest(reason: String): Unit = {
      logger.info(s"Reply bad request: $reason")
      response.setStatusCode(HttpURLConnection.HTTP_BAD_REQUEST, reason)
    }

    def replyOk[T: io.circe.Encoder](result: T): Unit = {
      response.setContentType("application/json; charset=utf-8")
      response.getWriter.write(result.asJson(implicitly[io.circe.Encoder[T]]).toString)
      response.setStatusCode(HttpURLConnection.HTTP_OK)
    }

    def withTmpDir(f: Path => Unit): Unit = {
      def remove(f: File): Unit = {
        if (f.isDirectory) { f.listFiles foreach remove }
        f.delete()
      }
      val tmpPath = Files.createTempDirectory("compile").toAbsolutePath
      try {
        f(tmpPath)
      } finally {
        remove(tmpPath.toFile)
      }
    }

    def handleCompile(tmpDir: Path, args: Seq[String], iFileSet: Set[String]): Unit = {

      case class Message(
          file: String,
          line: Int,
          start: Int,
          end: Int,
          point: Int,
          category: String,
          lines: Seq[String],
          context: String)

      case class Result(
          code: String,
          outputFiles: Map[String, String],
          messages: Seq[Message])

      replyOk {
        implicit val messageBuffer: MessageBuffer = new MessageBuffer

        val tmpDirName = tmpDir.toString
        val tmpDirPrefix = tmpDirName + File.separator

        def messages: Seq[Message] = messageBuffer.messages map { m =>
          Message(
            if (m.loc eq Loc.unknown) "" else m.loc.file.stripPrefix(tmpDirPrefix),
            m.loc.line,
            m.loc.start,
            m.loc.end,
            m.loc.point,
            m.category match {
              // $COVERAGE-OFF$ Trivial
              case Messages.WarningCategory => "WARNING"
              case Messages.ErrorCategory   => "ERROR"
              case Messages.NoteCategory    => "NOTE"
              case Messages.FatalCategory   => "FATAL"
              case Messages.IceCategory     => "INTERNAL COMPILER ERROR"
              // $COVERAGE-ON$
            },
            m.msg map { _.replaceAll(tmpDirPrefix, "").replaceAll(tmpDirName, ".") },
            m.context
          )
        }

        try {
          Compiler.parseArgs(messageBuffer, args, Some(tmpDir)) flatMap {
            case (settings, source, params) =>
              CommandLineInterface.compile(args, settings, source, params) map { _ =>
                val oDir = settings.oPath.get
                // Read all output files
                // TODO: Ignore input files if odir == tmpdir
                val files = if (oDir.toFile.exists) {
                  Map from {
                    Files.walk(oDir).iterator.asScala filter { path =>
                      // Return only regular files. Also ignore input files, in
                      // case the used specified the output directory to overlap
                      // and contain one of the input files (e.g.: '-o .')
                      path.toFile.isFile && !(iFileSet contains path.toFile.getCanonicalPath)
                    } map { file =>
                      file.toString.stripPrefix(tmpDirPrefix) -> Source(file.toFile).text
                    }
                  }
                } else {
                  Map.empty[String, String]
                }
                // Form result
                Result("ok", files, messages)
              }
          } getOrElse {
            // Argument parsing had errors
            Result("ok", Map.empty, messages)
          }
        } catch {
          case t: Throwable =>
            // TODO: Archive input for debug
            val sw = new StringWriter
            t.printStackTrace(new PrintWriter(sw))
            val trace = Message("", 0, 0, 0, 0, "STDERR", Nil, sw.toString)
            Result("crash", Map.empty, List(trace))
        }
      }
    }

    def handle(command: String, request: Json): Unit = {
      command match {
        case "describe" =>
          //////////////////////////////////////////////////////////////////////
          // WARNING: At a minimum, response to 'describe' must have the
          // apiVersion key so the client knows what to do next
          //////////////////////////////////////////////////////////////////////
          case class Describe(apiVersion: Int, compilerVersion: String, buildTime: String)
          replyOk {
            Describe(apiVersion, BuildInfo.version, BuildInfo.buildTime.toString)
          }
        case "compile" =>
          request.hcursor.get[JsonObject]("inputFiles") match {
            case Left(_) => replyBadRequest("No input files")
            case Right(files) =>
              withTmpDir { tmpDir =>
                assert(tmpDir.isAbsolute)
                // Write all input files to the temporary directory
                val (hasBadIFile, iFileSet) =
                  files.toIterable.iterator.foldLeft((false, Set.empty[String])) {
                    case (acc @ (true, _), _) => acc // Had error already
                    case ((false, files), (fileName, fileContent)) =>
                      fileContent.as[String] match {
                        case Left(_) => (true, files)
                        case Right(text) =>
                          val file = tmpDir.resolve(fileName).toFile
                          file.getParentFile.mkdirs()
                          file.createNewFile()
                          val w = new PrintWriter(file)
                          w.write(text)
                          w.close()
                          (false, files + file.getCanonicalPath)
                      }
                  }
                if (hasBadIFile) {
                  replyBadRequest("Bad input files")
                } else {
                  request.hcursor.get[String]("args") match {
                    case Left(_) =>
                      replyBadRequest("No args")
                    case Right(args) =>
                      handleCompile(tmpDir, args.trim.split("\\s+").toSeq, iFileSet)
                  }
                }
              }
          }
        case other =>
          replyBadRequest(s"Unknown request type: '$other'")
      }
    }

    // Set CORS headers. Allow access from anywhere.
    response.appendHeader("Access-Control-Allow-Origin", "*");

    request.getMethod match {
      case "OPTIONS" =>
        // Set CORS headers. Allow POST with the Content-Type header and caches
        // preflight response for 3600s.
        response.appendHeader("Access-Control-Allow-Methods", "POST");
        response.appendHeader("Access-Control-Allow-Headers", "Content-Type");
        response.appendHeader("Access-Control-Max-Age", "3600");
        response.setStatusCode(HttpURLConnection.HTTP_NO_CONTENT);
      case "POST" =>
        request.getContentType.toScala map { _.toLowerCase } match {
          case Some("application/json; charset=utf-8") =>
            val text = request.getReader.lines.collect(Collectors.joining("\n"))
            io.circe.parser.parse(text) match {
              case Left(_) =>
                replyBadRequest("Request must be a JSON object")
              case Right(json) =>
                json.hcursor.downField("request").focus match {
                  case None => replyBadRequest("Request must have 'request' key")
                  case Some(focus) =>
                    focus.as[String] match {
                      case Left(_)        => replyBadRequest("Value of 'request' must be a string")
                      case Right(command) => handle(command, json)
                    }
                }
            }
          case t =>
            logger.info(s"Reply unsupported content type: $t")
            response.setStatusCode(HttpURLConnection.HTTP_UNSUPPORTED_TYPE)
        }
      case _ =>
        logger.info("Reply bad method")
        response.setStatusCode(HttpURLConnection.HTTP_BAD_METHOD)
    }
  }

}
