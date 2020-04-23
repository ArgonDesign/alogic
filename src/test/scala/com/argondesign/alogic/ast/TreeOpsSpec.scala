////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Tests for TreeOps
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.ast

import com.argondesign.alogic.AlogicTest
import com.argondesign.alogic.ast.Trees.ExprBinary
import com.argondesign.alogic.ast.Trees._
import com.argondesign.alogic.core.Types.TypeUInt
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable

final class TreeOpsSpec extends AnyFreeSpec with AlogicTest {

  private val tree = Thicket(
    List(
      ExprBinary(
        ExprStr("0"),
        "a",
        ExprBinary(
          ExprStr("1"),
          "b",
          ExprStr("2")
        )
      ),
      DescVar(
        Ident("x", Nil),
        ExprType(TypeUInt(1)),
        None
      ),
      DescVar(
        Ident("y", Nil),
        ExprType(TypeUInt(2)),
        Some(
          ExprCat(
            List(
              ExprStr("3"),
              ExprBinary(
                ExprStr("4"),
                "c",
                ExprStr("5")
              ),
              ExprStr("6"),
              ExprStr("7")
            )
          )
        )
      )
    )
  )

  "Tree" - {
    "preOrderIterator should" - {
      "yield the whole tree in pre-order" in {
        val nodes = tree.preOrderIterator.toList

        nodes should have length 19
        nodes(0) should matchPattern { case Thicket(List(_, _, _)) => }
        nodes(1) should matchPattern { case ExprBinary(_, "a", _)  => }
        nodes(2) shouldBe ExprStr("0")
        nodes(3) should matchPattern { case ExprBinary(_, "b", _) => }
        nodes(4) shouldBe ExprStr("1")
        nodes(5) shouldBe ExprStr("2")
        nodes(6) should matchPattern { case DescVar(Ident("x", Nil), _, None) => }
        nodes(7) shouldBe Ident("x", Nil)
        nodes(8) shouldBe ExprType(TypeUInt(1))
        nodes(9) should matchPattern { case DescVar(Ident("y", Nil), _, Some(_)) => }
        nodes(10) shouldBe Ident("y", Nil)
        nodes(11) shouldBe ExprType(TypeUInt(2))
        nodes(12) should matchPattern { case ExprCat(List(_, _, _, _)) => }
        nodes(13) shouldBe ExprStr("3")
        nodes(14) should matchPattern { case ExprBinary(_, "c", _) => }
        nodes(15) shouldBe ExprStr("4")
        nodes(16) shouldBe ExprStr("5")
        nodes(17) shouldBe ExprStr("6")
        nodes(18) shouldBe ExprStr("7")
      }
    }

    "postOrderIterator should" - {
      "yield the wole tree in post-order" in {
        val nodes = tree.postOrderIterator.toList

        nodes should have length 19
        nodes(0) shouldBe ExprStr("0")
        nodes(1) shouldBe ExprStr("1")
        nodes(2) shouldBe ExprStr("2")
        nodes(3) should matchPattern { case ExprBinary(_, "b", _) => }
        nodes(4) should matchPattern { case ExprBinary(_, "a", _) => }
        nodes(5) shouldBe Ident("x", Nil)
        nodes(6) shouldBe ExprType(TypeUInt(1))
        nodes(7) should matchPattern { case DescVar(Ident("x", Nil), _, None) => }
        nodes(8) shouldBe Ident("y", Nil)
        nodes(9) shouldBe ExprType(TypeUInt(2))
        nodes(10) shouldBe ExprStr("3")
        nodes(11) shouldBe ExprStr("4")
        nodes(12) shouldBe ExprStr("5")
        nodes(13) should matchPattern { case ExprBinary(_, "c", _) => }
        nodes(14) shouldBe ExprStr("6")
        nodes(15) shouldBe ExprStr("7")
        nodes(16) should matchPattern { case ExprCat(List(_, _, _, _))            => }
        nodes(17) should matchPattern { case DescVar(Ident("y", Nil), _, Some(_)) => }
        nodes(18) should matchPattern { case Thicket(List(_, _, _))               => }
      }
    }

    "visit should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          val text = new mutable.StringBuilder
          tree visit {
            case ExprBinary(_, op, _) => text append op
          }
          text.toString shouldBe "ac"
        }

        "2" in {
          val text = new mutable.StringBuilder
          tree visit {
            case ExprStr(string) => text append string
          }
          text.toString shouldBe "01234567"
        }
      }
    }

    "visitAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          val text = new mutable.StringBuilder
          tree visitAll {
            case ExprBinary(_, op, _) => text append op
          }
          text.toString shouldBe "abc"
        }

        "2" in {
          val text = new mutable.StringBuilder
          tree visitAll {
            case ExprBinary(_, op, _) => text append op
            case ExprStr(string)      => text append string
          }
          text.toString shouldBe "a0b123c4567"
        }
      }
    }

    "collect should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          tree collect {
            case ExprBinary(_, op, _) => op
          } mkString "-" shouldBe "a-c"
        }

        "2" in {
          tree collect {
            case ExprStr(string) => string
          } mkString "-" shouldBe "0-1-2-3-4-5-6-7"
        }
      }

      "return an empty iterable when nothing matched" in {
        tree collect {
          case ExprStr("99") => fail
        } shouldBe empty
      }
    }

    "collectAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          tree collectAll {
            case ExprBinary(_, op, _) => op
          } mkString "-" shouldBe "a-b-c"
        }

        "2" in {
          tree collectAll {
            case ExprBinary(_, op, _) => op
            case ExprStr(string)      => string
          } mkString "-" shouldBe "a-0-b-1-2-3-c-4-5-6-7"
        }
      }

      "return an empty iterable when nothing matched" in {
        tree collectAll {
          case ExprStr("99") => fail
        } shouldBe empty
      }
    }

    "flatCollect should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          tree flatCollect {
            case ExprBinary(_, op, _) => List(op, "s")
          } mkString "-" shouldBe "a-s-c-s"
        }

        "2" in {
          tree flatCollect {
            case ExprStr(string) => List(string, "s")
          } mkString "-" shouldBe "0-s-1-s-2-s-3-s-4-s-5-s-6-s-7-s"
        }
      }

      "return an empty iterable when nothing matched" in {
        tree flatCollect {
          case ExprStr("99") => fail
        } shouldBe empty
      }
    }

    "flatCollectAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          tree flatCollectAll {
            case ExprBinary(_, op, _) => List(op, "s")
          } mkString "-" shouldBe "a-s-b-s-c-s"
        }

        "2" in {
          tree flatCollectAll {
            case ExprBinary(_, op, _) => List(op, "s")
            case ExprStr(string)      => List(string, "t")
          } mkString "-" shouldBe {
            "a-s-0-t-b-s-1-t-2-t-3-t-c-s-4-t-5-t-6-t-7-t"
          }
        }
      }

      "return an empty iterable when nothing matched" in {
        tree flatCollect {
          case ExprStr("99") => fail
        } shouldBe empty
      }
    }

    "collectFirst should" - {
      "walk nodes in pre-order and stop at the very first match" - {
        "1" in {
          (tree collectFirst {
            case ExprBinary(_, op, _) => op
          }).value shouldBe "a"
        }

        "2" in {
          (tree collectFirst {
            case ExprStr(string) => string
          }).value shouldBe "0"
        }
      }

      "return None when nothing matched" in {
        tree collectFirst {
          case ExprStr("99") => fail
        } shouldBe None
      }
    }

    "forall should" - {
      "walk nodes in pre-order and stop when matching nodes are found" in {
        tree forall {
          case ExprBinary(_, op, _) => op == "a" || op == "c"
        } shouldBe true
      }

      "return true when nothing matched" in {
        tree forall {
          case ExprStr("99") => fail
        } shouldBe true
      }
    }

    "forallAll should" - {
      "walk nodes in any order and keep going, checking all nodes" in {
        tree forallAll {
          case ExprBinary(_, op, _) => op == "a" || op == "c"
        } shouldBe false
      }

      "return true when nothing matched" in {
        tree forallAll {
          case ExprStr("99") => fail
        } shouldBe true
      }
    }

    "exists should" - {
      "walk nodes in pre-order and stop when matching nodes are found" in {
        tree exists {
          case ExprBinary(_, op, _) => op == "b"
        } shouldBe false
      }

      "return false when nothing matched" in {
        tree exists {
          case ExprStr("99") => fail
        } shouldBe false
      }
    }

    "existsAll should" - {
      "walk nodes in any order and keep going, checking all nodes" in {
        tree existsAll {
          case ExprBinary(_, op, _) => op == "b"
        } shouldBe true
      }

      "return false when nothing matched" in {
        tree existsAll {
          case ExprStr("99") => fail
        } shouldBe false
      }
    }
  }
}
