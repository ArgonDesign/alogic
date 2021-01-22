////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Tests for Google Cloud Function endpoint interface
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.gcp

import com.argondesign.alogic.BuildInfo
import com.google.cloud.functions.HttpRequest
import com.google.cloud.functions.HttpResponse
import io.circe.syntax._
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io._
import java.net.HttpURLConnection
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.ChainingSyntax

final class FunctionCompileSpec
    extends AnyFreeSpec
    with MockFactory
    with Matchers
    with OptionValues
    with ChainingSyntax {

  private def mkReader(text: String) = new BufferedReader(new StringReader(text))

  private class ResponseWriter private (val out: Writer) extends BufferedWriter(out) {
    def this() = this(new StringWriter())

    def getText: String = {
      flush()
      out.toString
    }

  }

  private def mkWriter = new ResponseWriter()

  private val requestHeaders = Map.empty[String, java.util.List[String]].asJava

  private def mkRequest(text: String): HttpRequest =
    mock[HttpRequest] tap { request =>
      (request.getHeaders _).expects().returns(requestHeaders)
      (request.getMethod _).expects().returns("POST")
      (request.getContentType _).expects().returns(Some("application/json; charset=utf-8").toJava)
      (request.getReader _).expects().returns(mkReader(text))
    }

  private def checkBadRequest(request: HttpRequest)(reason: String): Unit = {
    val response = mock[HttpResponse]
    (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
    (response.setStatusCode(_: Int, _: String)).expects(HttpURLConnection.HTTP_BAD_REQUEST, reason)
    (new FunctionCompile).service(request, response)
  }

  private def checkOK(request: HttpRequest)(expected: String): Unit = {
    val response = mock[HttpResponse]
    (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
    (response.setContentType(_: String)).expects("application/json; charset=utf-8")
    (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
    val writer = mkWriter
    (response.getWriter _).expects().returns(writer)

    (new FunctionCompile).service(request, response)

    val resultJson = io.circe.parser.parse(writer.getText).fold(_ => fail, identity)
    val expectedJson = io.circe.parser.parse(expected).fold(_ => fail, identity)

    resultJson shouldBe expectedJson
  }

  // TODO: Factor this together with the one in CompilationTest
  private def checkJson(json: io.circe.Json, expected: Map[String, String]): Unit =
    expected foreach {
      case (key, value) =>
        @tailrec
        def select(key: String, cursor: io.circe.ACursor): io.circe.ACursor = if (key.isEmpty) {
          cursor
        } else {
          val (first, rest) = {
            def splitEscaped(string: String): (String, String) = {
              val (first, rest) = string.span(_ != '.')
              if (first.endsWith("\\")) {
                val (subFirst, subRest) = splitEscaped(rest.tail)
                (first.init + "." + subFirst, subRest)
              } else {
                (first, rest.tail)
              }
            }
            splitEscaped(key)
          }
          val (name, idx) = first.span(_ != '[')
          val fCursor = cursor.downField(name)
          select(rest, if (idx.isEmpty) fCursor else fCursor.downN(idx.tail.init.toInt))
        }

        select(key, json.hcursor).focus match {
          case None => fail(s"No entry '$key'")
          case Some(json) =>
            if (value == "*") {
              // Wildcard: OK
            } else {
              val expectedValue = io.circe.parser.parse(value).fold(_ => fail, identity)
              assert(json === expectedValue, s"'$key''")
            }
        }
    }

  "FunctionCompile" - {

    "should reject invalid HTTP method" - {
      for (method <- List("GET", "PUT", "HEAD", "DELETE", "PATCH", "TRACE", "CONNECT")) {
        method in {
          val request = stub[HttpRequest]
          (request.getHeaders _).when().returns(requestHeaders)
          (request.getMethod _).when().returns(method)

          val response = mock[HttpResponse]
          (response.appendHeader(_: String, _: String)).stubs(*, *)
          (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_BAD_METHOD)

          (new FunctionCompile).service(request, response)
        }
      }
    }

    "should respond to OPTIONS request with CORS headers" in {
      val request = stub[HttpRequest]
      (request.getHeaders _).when().returns(requestHeaders)
      (request.getMethod _).when().returns("OPTIONS")

      val response = mock[HttpResponse]
      (response
        .appendHeader(_: String, _: String))
        .expects("Access-Control-Allow-Origin", "*")
      (response
        .appendHeader(_: String, _: String))
        .expects("Access-Control-Allow-Methods", "POST")
      (response
        .appendHeader(_: String, _: String))
        .expects("Access-Control-Allow-Headers", "Content-Type")
      (response
        .appendHeader(_: String, _: String))
        .expects("Access-Control-Max-Age", "3600")
      (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_NO_CONTENT)

      (new FunctionCompile).service(request, response)
    }

    "should reject invalid content type" - {
      for (
        content <- List(
          None, // Unspecified
          Some(""), // Empty
          Some("application/json"), // Without charset
          Some("application/json; charset=ascii") // not UTF-8 charset
        )
      ) {
        content.toString in {
          val request = stub[HttpRequest]
          (request.getHeaders _).when().returns(requestHeaders)
          (request.getMethod _).when().returns("POST")
          (request.getContentType _).when().returns(content.toJava)

          val response = mock[HttpResponse]
          (response.appendHeader(_: String, _: String)).stubs(*, *)
          (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_UNSUPPORTED_TYPE)

          (new FunctionCompile).service(request, response)
        }
      }
    }

    "should reject non-JSON request" in {
      val request = mkRequest("@@@-malformed")
      checkBadRequest(request)("Request must be a JSON object")
    }

    "should reject non JSON object request" in {
      val request = mkRequest("1")
      checkBadRequest(request)("Request must have 'request' key")
    }

    "should reject JSON object requests with no 'request' key" in {
      val request = mkRequest("""{ "foo" : 0 }""")
      checkBadRequest(request)("Request must have 'request' key")
    }

    "should reject JSON object requests with 'request' not a string" in {
      val request = mkRequest("""{ "request" : 0 }""")
      checkBadRequest(request)("Value of 'request' must be a string")
    }

    "should reject unkown requests" in {
      val request = mkRequest("""{ "request": "foo" }""")
      checkBadRequest(request)("Unknown request type: 'foo'")
    }

    "should respond to 'describe'" in {
      val request = mkRequest("""{ "request" : "describe" }""")
      checkOK(request) {
        s"""{
           |  "apiVersion": 1,
           |  "compilerVersion": "${BuildInfo.version}",
           |  "buildTime": "${BuildInfo.buildTime}"
           |}""".stripMargin
      }
    }

    "response to 'describe' must always have 'apiVersion'" in {
      // WARNING: Do not change this test (except for the actual version number).
      // 'apiVersion' is required so the client knows what to do next!
      val request = mkRequest("""{ "request" : "describe" }""")

      val response = mock[HttpResponse]
      (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
      (response.setContentType(_: String)).expects("application/json; charset=utf-8")
      (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
      val writer = mkWriter
      (response.getWriter _).expects().returns(writer)

      (new FunctionCompile).service(request, response)

      io.circe.parser.parse(writer.getText) match {
        case Left(_)     => fail
        case Right(json) => json.hcursor.get[Int]("apiVersion").toOption.value shouldBe 1
      }
    }

    "should respond to 'compile'" - {
      "valid request" in {
        val sourceExample =
          """|network example {
             |  in bool i;
             |  out bool o;
             |  i -> o;
             |}""".stripMargin

        val request = mkRequest {
          s"""{
             |  "request" : "compile",
             |  "inputFiles" : {
             |    "example.alogic" : ${sourceExample.asJson}
             |  },
             |  "args" : "-o out example.alogic"
             |}""".stripMargin
        }

        val response = mock[HttpResponse]
        (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
        (response.setContentType(_: String)).expects("application/json; charset=utf-8")
        (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
        val writer = mkWriter
        (response.getWriter _).expects().returns(writer)

        (new FunctionCompile).service(request, response)

        val resultJson = io.circe.parser.parse(writer.getText).fold(_ => fail, identity)

        checkJson(
          resultJson,
          Map(
            "code" -> "\"ok\"",
            "outputFiles.out/example\\.v" -> "*",
            "outputFiles.out/manifest\\.json" -> "*",
            "messages" -> "[]"
          )
        )
      }

      "valid request with type error" in {
        val sourceExample =
          """|network example {
             |  in  u1 i;
             |  out u2 o;
             |  i -> o;
             |}""".stripMargin

        val request = mkRequest {
          s"""{
             |  "request" : "compile",
             |  "inputFiles" : {
             |    "example.alogic" : ${sourceExample.asJson}
             |  },
             |  "args" : "-o out example.alogic"
             |}""".stripMargin
        }

        checkOK(request) {
          """{
            |  "code": "ok",
            |  "outputFiles": {},
            |  "messages": [
            |    {
            |      "file" : "example.alogic",
            |      "line" : 4,
            |      "start" : 44,
            |      "end" : 50,
            |      "point" : 49,
            |      "category" : "ERROR",
            |      "lines" : [
            |        "Connected ports have mismatched widths: 1 -> 2"
            |      ],
            |      "context" : "  i -> o;\n  ~~~~~^"
            |    }
            |  ]
            |}""".stripMargin
        }
      }

      "no input files" in {
        val request = mkRequest {
          """{
            |  "request" : "compile",
            |  "args" : ""
            |}""".stripMargin
        }

        checkBadRequest(request)("No input files")
      }

      "bad input file" in {
        val request = mkRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : { "a": true, "b": "" },
            |  "args" : ""
            |}""".stripMargin
        }

        checkBadRequest(request)("Bad input files")
      }

      "no args" in {
        val request = mkRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : {}
            |}""".stripMargin
        }

        checkBadRequest(request)("No args")
      }

      "bad args" in {
        val request = mkRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : {},
            |  "args" : "--nonexistent"
            |}""".stripMargin
        }

        checkOK(request) {
          """{
            |  "code" : "ok",
            |  "outputFiles" : {},
            |  "messages" : [
            |    {
            |      "file" : "",
            |      "line" : 0,
            |      "start" : 0,
            |      "end" : 0,
            |      "point" : 0,
            |      "category" : "ERROR",
            |      "lines" : [
            |         "Unknown option 'nonexistent'"
            |       ],
            |      "context" : ""
            |    }
            |  ]
            |}""".stripMargin
        }
      }

    }

    "should report compiler crash" in {
      val request = mkRequest {
        s"""{
           |  "request" : "compile",
           |  "inputFiles" : {},
           |  "args" : "--test-crash -o . ."
           |}""".stripMargin
      }

      val response = mock[HttpResponse]
      (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
      (response.setContentType(_: String)).expects("application/json; charset=utf-8")
      (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
      val writer = mkWriter
      (response.getWriter _).expects().returns(writer)

      (new FunctionCompile).service(request, response)

      val resultJson = io.circe.parser.parse(writer.getText).fold(_ => fail, identity)

      checkJson(
        resultJson,
        Map(
          "code" -> "\"crash\"",
          "outputFiles" -> "{}",
          "messages[0].file" -> "\"\"",
          "messages[0].category" -> "\"STDERR\""
        )
      )
    }

    "should signal error for input file outside sandbox" - {
      List("../top.algoic", "/top.algoci") foreach { path =>
        path in {
          val request = mkRequest {
            s"""{
               |  "request" : "compile",
               |  "inputFiles" : {
               |    "$path" : ""
               |  },
               |  "args" : ""
               |}""".stripMargin
          }

          val response = mock[HttpResponse]
          (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
          (response.setContentType(_: String)).expects("application/json; charset=utf-8")
          (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
          val writer = mkWriter
          (response.getWriter _).expects().returns(writer)

          (new FunctionCompile).service(request, response)

          val resultJson = io.circe.parser.parse(writer.getText).fold(_ => fail, identity)

          checkJson(
            resultJson,
            Map(
              "code" -> "\"invalid-input\"",
              "outputFiles" -> "{}",
              "messages[0].file" -> "\"\"",
              "messages[0].category" -> "\"ERROR\"",
              "messages[0].lines[0]" -> s""""Input file $path is outside sandbox""""
            )
          )
        }
      }
    }

    "should signal error for import from outside sandbox" in {
      val request = mkRequest {
        s"""{
           |  "request" : "compile",
           |  "inputFiles" : {
           |    "top.alogic" : "import \\"../a.alogic\\" as a;",
           |    "../a.alogic" : ""
           |  },
           |  "args" : "-o out top.alogic"
           |}""".stripMargin
      }

      val response = mock[HttpResponse]
      (response.appendHeader(_: String, _: String)).expects("Access-Control-Allow-Origin", "*")
      (response.setContentType(_: String)).expects("application/json; charset=utf-8")
      (response.setStatusCode(_: Int)).expects(HttpURLConnection.HTTP_OK)
      val writer = mkWriter
      (response.getWriter _).expects().returns(writer)

      (new FunctionCompile).serviceInternal(request, response, allowInputOutsideSanbox = true)

      val resultJson = io.circe.parser.parse(writer.getText).fold(_ => fail, identity)

      checkJson(
        resultJson,
        Map(
          "code" -> "\"ok\"",
          "outputFiles" -> "{}",
          "messages[0].file" -> "\"top.alogic\"",
          "messages[0].line" -> "1",
          "messages[0].category" -> "\"ERROR\"",
          "messages[0].lines[0]" -> "\"Imported file is outside sandbox\""
        )
      )
    }

  }

}
