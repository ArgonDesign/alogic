////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
//   UnusedCheck tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.frontend

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Loc
import com.argondesign.alogic.core.Messages.Warning
import com.argondesign.alogic.core.Source
import org.scalatest.flatspec.AnyFlatSpec

final class UnusedCheckSpec extends AnyFlatSpec with AlogicTest {

  implicit val cc: CompilerContext = new CompilerContext

  private def check(text: String): Tree = {
    val source = Source("UnusedCheckSpec", text)
    val fe = new Frontend
    val result = fe(source, Loc.synthetic, Nil).value
    result._2.size shouldBe 0
    result._1
  }

  "UnusedCheck" should "issue warning for unused local variables" in {
    check("fsm a { i8 b; }")
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused local const variables" in {
    check("fsm a { const i8 b = 0; }")
    cc.messages.loneElement should beThe[Warning]("Constant 'b' is unused")
  }

  it should "not issue warning for variable used in nested scope" in {
    check("void foo() { i8 b; { @display(\"\", b); } }")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entity variables" in {
    check("fsm a { i8 b; }")
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused arrays" in {
    check("fsm a { i8 b[2]; }")
    cc.messages.loneElement should beThe[Warning]("Array 'b' is unused")
  }

  it should "issue warning for unused input ports" in {
    check("fsm a { in i8 b; }")
    cc.messages.loneElement should beThe[Warning]("Input port 'b' is unused")
  }

  it should "issue warning for unused input ports - but not in verbatim entity" in {
    check("verbatim entity a { in i8 b; }")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused output ports" in {
    check("fsm a { out i8 b; }")
    cc.messages.loneElement should beThe[Warning]("Output port 'b' is unused")
  }

  it should "issue warning for unused output ports - but not in verbatim entity" in {
    check("verbatim entity a { out i8 b; }")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused parameters" in {
    check {
      """network c {
        |  out bool o;
        |  fsm a {
        |    param i8 b = 8'd9;
        |    void main() {
        |      o = 1;
        |      fence;
        |    }
        |  }
        |  b = new a();
        |}""".stripMargin
    }
    cc.messages.loneElement should beThe[Warning]("Parameter 'b' is unused")
  }

  it should "issue warning for unused parameters - but not in verbatim entity" in {
    check {
      """network c {
        |  out bool o;
        |  verbatim entity a {
        |    out bool o;
        |    param i8 b = 8'd9;
        |  }
        |  b = new a();
        |  b.o -> o;
        |}""".stripMargin
    }
    cc.messages shouldBe empty
  }

  it should "issue warning for unused type parameters" in {
    check {
      """network c {
        |  out bool o;
        |  fsm a {
        |    param type b = bool;
        |    void main() {
        |      o = 1;
        |      fence;
        |    }
        |  }
        |  b = new a();
        |}""".stripMargin
    }
    cc.messages.loneElement should beThe[Warning]("Type parameter 'b' is unused")
  }

  it should "issue warning for unused constants" in {
    check("fsm a { const i8 b = 8'd9; }")
    cc.messages.loneElement should beThe[Warning]("Constant 'b' is unused")
  }

  it should "issue warning for unused pipeline variables" in {
    check("network a { pipeline i8 b; }")
    cc.messages.loneElement should beThe[Warning]("Pipeline variable 'b' is unused")
  }

  it should "issue warning for unused SRAMs" in {
    check("fsm a { sram i8 b[1]; }")
    cc.messages.loneElement should beThe[Warning]("SRAM 'b' is unused")
  }

  it should "issue warning for unused functions" in {
    check("fsm a { void foo() { fence; } }")
    cc.messages.loneElement should beThe[Warning]("Function 'foo' is unused")
  }

  it should "issue warning for unused functions - but not for main" in {
    check("fsm a { void main() { fence; } }")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entities" in {
    check("network a { fsm bar {} }")
    cc.messages.loneElement should beThe[Warning]("Entity 'bar' is unused")
  }

  it should "issue warning for unused singleton instances" in {
    check("network a { new fsm bar {} }")
    cc.messages.loneElement should beThe[Warning]("Singleton instance 'bar' is unused")
  }

  it should "not issue warning for unused struct in file scope" in {
    check("struct s {bool f;} network a {}")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused struct in entity scope" in {
    check("network a {struct s {bool f;}}")
    cc.messages.loneElement should beThe[Warning]("struct 's' is unused")
  }

  it should "not issue warning for unused typedef in file scope" in {
    check("typedef bool t; network a {}")
    cc.messages shouldBe empty
  }

  it should "issue warning for unused typedef in entity scope" in {
    check("network a {typedef bool t;}")
    cc.messages.loneElement should beThe[Warning]("Type 't' is unused")
  }

  it should "not issue unused warning for nested instance using outer ports" in {
    check {
      """network foo {
        |  in  sync ready void i;
        |  out sync ready void o;
        |  new fsm bar {
        |    void main() {
        |      i.read();
        |      fence;
        |      o.write();
        |      fence;
        |    }
        |  }
        |}""".stripMargin
    }
    cc.messages shouldBe empty
  }

  it should "issue unused warnings in source line order" in {
    check {
      """void f() {
        |  bool a;
        |  {
        |    bool b;
        |    {
        |      bool c;
        |    }
        |  }
        |}""".stripMargin
    }

    cc.messages should have length 3
    cc.messages(0) should beThe[Warning]("Variable 'a' is unused")
    cc.messages(0).loc.line shouldBe 2
    cc.messages(1) should beThe[Warning]("Variable 'b' is unused")
    cc.messages(1).loc.line shouldBe 4
    cc.messages(2) should beThe[Warning]("Variable 'c' is unused")
    cc.messages(2).loc.line shouldBe 6
  }
}
