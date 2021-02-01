////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Tests for Google Cloud Function endpoint interface
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.gcp

import com.argondesign.alogic.BuildInfo
import com.argondesign.alogic.CheckJson
import com.argondesign.alogic.MockitoSugar._
import com.google.cloud.functions.HttpRequest
import com.google.cloud.functions.HttpResponse
import io.circe.syntax._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.io._
import java.net.HttpURLConnection
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.ChainingSyntax

final class FunctionCompileSpec
    extends AnyFreeSpec
    with Matchers
    with OptionValues
    with ChainingSyntax {

  private val requestHeaders = Map.empty[String, java.util.List[String]].asJava

  private def mockRequest(text: String): HttpRequest =
    mock[HttpRequest] tap { request =>
      request.getHeaders willReturn requestHeaders
      request.getMethod willReturn "POST"
      request.getContentType willReturn Some("application/json; charset=utf-8").toJava
      request.getReader willReturn new BufferedReader(new StringReader(text))
    }

  private def checkBadRequest(request: HttpRequest)(reason: String): Unit = {
    val response = mock[HttpResponse]

    (new FunctionCompile).service(request, response)

    response verify { __ =>
      __.appendHeader("Access-Control-Allow-Origin", "*")
      __.setStatusCode(HttpURLConnection.HTTP_BAD_REQUEST, reason)
    }
  }

  case class checkOk(request: HttpRequest) {

    val (response, getContent) = mock[HttpResponse] pipe { response =>
      val stringWriter = new StringWriter
      val bufferedWriter = new BufferedWriter(stringWriter)
      response.getWriter willReturn bufferedWriter
      (response, () => { bufferedWriter.flush(); stringWriter.toString })
    }

    private def verify(): Unit = response verifyIgnoringStubs { __ =>
      __.appendHeader("Access-Control-Allow-Origin", "*")
      __.setContentType("application/json; charset=utf-8")
      __.setStatusCode(HttpURLConnection.HTTP_OK)
    }

    protected val servicerOpt: Option[(HttpRequest, HttpResponse) => Unit] = None

    private def getServicer = servicerOpt getOrElse { (new FunctionCompile).service(_, _) }

    def apply(expected: String): Unit = {
      getServicer(request, response)
      verify()
      val resultJson = io.circe.parser.parse(getContent()).fold(_ => fail(), identity)
      val expectedJson = io.circe.parser.parse(expected).fold(_ => fail(), identity)
      resultJson shouldBe expectedJson
    }

    def apply(expected: (String, String)*): Unit = {
      getServicer(request, response)
      verify()
      val resultJson = io.circe.parser.parse(getContent()).fold(_ => fail(), identity)
      CheckJson(resultJson, expected.toMap)
    }

    def withServicer(servicer: (HttpRequest, HttpResponse) => Unit): checkOk =
      new checkOk(request) {
        override val servicerOpt: Option[(HttpRequest, HttpResponse) => Unit] = Some(servicer)
      }

  }

  "FunctionCompile" - {

    "should reject invalid HTTP method" - {
      for (method <- List("GET", "PUT", "HEAD", "DELETE", "PATCH", "TRACE", "CONNECT")) {
        method in {
          val request = mock[HttpRequest]
          request.getHeaders willReturn requestHeaders
          request.getMethod willReturn method

          val response = mock[HttpResponse]

          (new FunctionCompile).service(request, response)

          response verify { __ =>
            __.setStatusCode(HttpURLConnection.HTTP_BAD_METHOD)
            __.appendHeader(*, *)
          }
        }
      }
    }

    "should respond to OPTIONS request with CORS headers" in {
      val request = mock[HttpRequest]
      request.getHeaders willReturn requestHeaders
      request.getMethod willReturn "OPTIONS"

      val response = mock[HttpResponse]

      (new FunctionCompile).service(request, response)

      response verify { __ =>
        __.appendHeader("Access-Control-Allow-Origin", "*")
        __.appendHeader("Access-Control-Allow-Methods", "POST")
        __.appendHeader("Access-Control-Allow-Headers", "Content-Type")
        __.appendHeader("Access-Control-Max-Age", "3600")
        __.setStatusCode(HttpURLConnection.HTTP_NO_CONTENT)
      }
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
          val request = mock[HttpRequest]
          request.getHeaders willReturn requestHeaders
          request.getMethod willReturn "POST"
          request.getContentType willReturn content.toJava

          val response = mock[HttpResponse]

          (new FunctionCompile).service(request, response)

          response verify { __ =>
            __.appendHeader(*, *)
            __.setStatusCode(HttpURLConnection.HTTP_UNSUPPORTED_TYPE)
          }
        }
      }
    }

    "should reject non-JSON request" in {
      val request = mockRequest("@@@-malformed")
      checkBadRequest(request)("Request must be a JSON object")
    }

    "should reject non JSON object request" in {
      val request = mockRequest("1")
      checkBadRequest(request)("Request must have 'request' key")
    }

    "should reject JSON object requests with no 'request' key" in {
      val request = mockRequest("""{ "foo" : 0 }""")
      checkBadRequest(request)("Request must have 'request' key")
    }

    "should reject JSON object requests with 'request' not a string" in {
      val request = mockRequest("""{ "request" : 0 }""")
      checkBadRequest(request)("Value of 'request' must be a string")
    }

    "should reject unknown requests" in {
      val request = mockRequest("""{ "request": "foo" }""")
      checkBadRequest(request)("Unknown request type: 'foo'")
    }

    "should respond to 'describe'" in {
      val request = mockRequest("""{ "request" : "describe" }""")
      checkOk(request) {
        s"""{
              "apiVersion": 1,
              "compilerVersion": "${BuildInfo.version}",
              "buildTime": "${BuildInfo.buildTime}"
            }"""
      }
    }

    "response to 'describe' must always have 'apiVersion'" in {
      // WARNING: Do not change this test (except for the actual version
      // number) 'apiVersion' is required to be present so the client knows
      // what to do with the response!
      val request = mockRequest {
        """{
          | "request" : "describe"
          |}""".stripMargin
      }

      checkOk(request) {
        "apiVersion" -> "1"
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

        val request = mockRequest {
          s"""{
             |  "request" : "compile",
             |  "inputFiles" : {
             |    "example.alogic" : ${sourceExample.asJson}
             |  },
             |  "args" : "-o out example.alogic"
             |}""".stripMargin
        }

        checkOk(request)(
          "code" -> "\"ok\"",
          "outputFiles|out/example.v" -> "*",
          "outputFiles|out/manifest.json" -> "*",
          "messages" -> "[]"
        )
      }

      "valid request with type error" in {
        val sourceExample =
          """|network example {
             |  in  u1 i;
             |  out u2 o;
             |  i -> o;
             |}""".stripMargin

        val request = mockRequest {
          s"""{
             |  "request" : "compile",
             |  "inputFiles" : {
             |    "example.alogic" : ${sourceExample.asJson}
             |  },
             |  "args" : "-o out example.alogic"
             |}""".stripMargin
        }

        checkOk(request) {
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
        val request = mockRequest {
          """{
            |  "request" : "compile",
            |  "args" : ""
            |}""".stripMargin
        }

        checkBadRequest(request)("No input files")
      }

      "bad input file" in {
        val request = mockRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : { "a": true, "b": "" },
            |  "args" : ""
            |}""".stripMargin
        }

        checkBadRequest(request)("Bad input files")
      }

      "no args" in {
        val request = mockRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : {}
            |}""".stripMargin
        }

        checkBadRequest(request)("No args")
      }

      "bad args" in {
        val request = mockRequest {
          """{
            |  "request" : "compile",
            |  "inputFiles" : {},
            |  "args" : "--nonexistent"
            |}""".stripMargin
        }

        checkOk(request) {
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
      val request = mockRequest {
        s"""{
           |  "request" : "compile",
           |  "inputFiles" : {},
           |  "args" : "--test-crash -o . ."
           |}""".stripMargin
      }

      checkOk(request)(
        "code" -> "\"crash\"",
        "outputFiles" -> "{}",
        "messages[0]|file" -> "\"\"",
        "messages[0]|category" -> "\"STDERR\""
      )
    }

    "should signal error for input file outside sandbox" - {
      List("../top.algoic", "/top.alogic") foreach { path =>
        path in {
          val request = mockRequest {
            s"""{
               |  "request" : "compile",
               |  "inputFiles" : {
               |    "$path" : ""
               |  },
               |  "args" : ""
               |}""".stripMargin
          }

          checkOk(request)(
            "code" -> "\"ok\"",
            "outputFiles" -> "{}",
            "messages[0]|file" -> "\"\"",
            "messages[0]|category" -> "\"ERROR\"",
            "messages[0]|lines[0]" -> s""""Input file $path is outside sandbox""""
          )
        }
      }
    }

    "should signal error for import from outside sandbox" in {
      val request = mockRequest {
        s"""{
           |  "request" : "compile",
           |  "inputFiles" : {
           |    "top.alogic" : "import \\"../a.alogic\\" as a;",
           |    "../a.alogic" : ""
           |  },
           |  "args" : "-o out top.alogic"
           |}""".stripMargin
      }

      checkOk(request).withServicer(
        (new FunctionCompile).serviceInternal(_, _, allowInputOutsideSandbox = true)
      )(
        "code" -> "\"ok\"",
        "outputFiles" -> "{}",
        "messages[0]|file" -> "\"top.alogic\"",
        "messages[0]|line" -> "1",
        "messages[0]|category" -> "\"ERROR\"",
        "messages[0]|lines[0]" -> "\"Imported file is outside sandbox\""
      )
    }

    "should signal timeout on long computation" in {
      val request = mockRequest {
        s"""{
           |  "request" : "compile",
           |  "inputFiles" : {
           |    "top.alogic" : ""
           |  },
           |  "args" : "-o out top.alogic"
           |}""".stripMargin
      }

      checkOk(request).withServicer(
        (new FunctionCompile).serviceInternal(_, _, timeoutMs = 0)
      )(
        "code" -> "\"timeout\"",
        "outputFiles" -> "{}",
        "messages" -> "[]"
      )
    }
  }
}
