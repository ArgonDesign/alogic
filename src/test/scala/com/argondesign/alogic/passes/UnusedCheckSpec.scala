////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018-2019 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// UnusedCheck tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.passes

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.SourceTextConverters._
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.CompilerContext
import com.argondesign.alogic.core.Warning
import org.scalatest.FlatSpec

final class UnusedCheckSpec extends FlatSpec with AlogicTest {

  implicit val cc = new CompilerContext

  def xform(tree: Tree) = {
    tree match {
      case Root(_, entity: Entity) => cc.addGlobalEntity(entity)
      case entity: Entity          => cc.addGlobalEntity(entity)
      case _                       =>
    }
    tree rewrite new Namer rewrite new UnusedCheck(postSpecialize = false)
  }

  "The UnusedCheck" should "issue warning for unused local variables" in {
    xform("{ i8 b; }".asTree[Stmt])
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused local variables - but not with 'unused' attribute" in {
    xform("{ (* unused *) i8 b; }".asTree[Stmt])
    cc.messages shouldBe empty
  }

  it should "not issue warning for variable used in nested scope" in {
    xform("{ i8 b; { b; } }".asTree[Stmt])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entity variables" in {
    xform("fsm a { i8 b; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Variable 'b' is unused")
  }

  it should "issue warning for unused entity variables - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused arrays" in {
    xform("fsm a { i8 b[2]; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Array 'b' is unused")
  }

  it should "issue warning for unused arrays - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) i8 b[2]; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused input ports" in {
    xform("fsm a { in i8 b; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Input port 'b' is unused")
  }

  it should "issue warning for unused input ports - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) in i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused input ports - but not in verbatim entity" in {
    xform("verbatim entity a { in i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused output ports" in {
    xform("fsm a { out i8 b; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Output port 'b' is unused")
  }

  it should "issue warning for unused output ports - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) out i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused output ports - but not in verbatim entity" in {
    xform("verbatim entity a { out i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused parameters" in {
    xform("fsm a { param i8 b = 8'd9; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Parameter 'b' is unused")
  }

  it should "issue warning for unused parameters - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) param i8 b = 8'd9; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused parameters - but not in verbatim entity" in {
    xform("verbatim entity a { param i8 b = 8'd9; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused constants" in {
    xform("fsm a { const i8 b = 8'd9; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Constant 'b' is unused")
  }

  it should "issue warning for unused constants - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) const i8 b = 8'd9; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused pipeline variables" in {
    xform("fsm a { pipeline i8 b; }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Pipeline variable 'b' is unused")
  }

  it should "issue warning for unused pipeline variables - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) pipeline i8 b; }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused functions" in {
    xform("fsm a { void foo() {} }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Function 'foo' is unused")
  }

  it should "issue warning for unused functions - but not for main" in {
    xform("fsm a { void main() {} }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused functions - but not with 'unused' attribute" in {
    xform("fsm a { (* unused *) void foo() {} }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused entities" in {
    xform("network a { fsm bar {} }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Entity 'bar' is unused")
  }

  it should "issue warning for unused entities - but not with 'unused' attribute" in {
    xform("network a { (* unused *) fsm bar {} }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused instances" in {
    xform("network a { new fsm bar {} }".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Instance 'bar' is unused")
  }

  it should "issue warning for unused instances - but not with 'unused' attribute" in {
    xform("network a { (* unused *) new fsm bar {} }".asTree[Entity])
    cc.messages shouldBe empty
  }

  it should "issue warning for unused struct - in file scope" in {
    xform("struct s {bool f;} network a {}".asTree[Root])
    cc.messages.loneElement should beThe[Warning]("struct 's' is unused")
  }

  it should "issue warning for unused struct - in entity scope" in {
    xform("network a {struct s {bool f;}}".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("struct 's' is unused")
  }

  it should "issue warning for unused typedef - in file scope" in {
    xform("typedef bool t; network a {}".asTree[Root])
    cc.messages.loneElement should beThe[Warning]("Type 't' is unused")
  }

  it should "issue warning for unused typedef - in entity scope" in {
    xform("network a {typedef bool t;}".asTree[Entity])
    cc.messages.loneElement should beThe[Warning]("Type 't' is unused")
  }

  it should "not issue unused warning for nested instance using outer ports" in {
    xform {
      """|network foo {
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
         |}""".stripMargin.asTree[Entity]
    }
    cc.messages shouldBe empty
  }

  it should "issue unused warnings in source line order" in {
    xform {
      """|{
         |  bool a;
         |  {
         |    bool b;
         |    {
         |      bool c;
         |    }
         |  }
         |}""".asTree[Stmt]
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
