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
// Tests for StructuredTree (and mostly TreeLike actually)
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lib

import com.argondesign.alogic.AlogicTest

import org.scalatest.FreeSpec

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.StringBuilder

final class StructuredTreeSpec extends FreeSpec with AlogicTest {

  trait TestTree extends StructuredTree

  case class TreeBin(lo: TestTree, hi: TestTree) extends TestTree
  case class TreeList(lst: List[TestTree]) extends TestTree
  case class TreeOpt(opt: Option[TestTree]) extends TestTree
  case class TreeLeaf(s: String) extends TestTree

  val tree = {
    TreeBin(
      TreeBin(
        TreeLeaf("0"),
        TreeBin(
          TreeLeaf("1"),
          TreeLeaf("2")
        )
      ),
      TreeOpt(
        Some(TreeList(
          List(
            TreeLeaf("3"),
            TreeOpt(None),
            TreeBin(
              TreeLeaf("4"),
              TreeLeaf("5")
            ),
            TreeLeaf("6"),
            TreeOpt(Some(TreeLeaf("7"))),
            TreeLeaf("8")
          )
        ))
      )
    )
  }

  "StructuredTree" - {

    "preOrderIterator should" - {
      "yield the wole tree in pre-order" in {
        val nodes = tree.preOrderIterator.toList

        nodes should have length 17
        nodes(0) shouldBe a[TreeBin]
        nodes(1) shouldBe a[TreeBin]
        nodes(2) shouldBe TreeLeaf("0")
        nodes(3) shouldBe a[TreeBin]
        nodes(4) shouldBe TreeLeaf("1")
        nodes(5) shouldBe TreeLeaf("2")
        nodes(6) shouldBe a[TreeOpt]
        nodes(7) shouldBe a[TreeList]
        nodes(8) shouldBe TreeLeaf("3")
        nodes(9) shouldBe a[TreeOpt]
        nodes(10) shouldBe a[TreeBin]
        nodes(11) shouldBe TreeLeaf("4")
        nodes(12) shouldBe TreeLeaf("5")
        nodes(13) shouldBe TreeLeaf("6")
        nodes(14) shouldBe a[TreeOpt]
        nodes(15) shouldBe TreeLeaf("7")
        nodes(16) shouldBe TreeLeaf("8")
      }
    }

    "postOrderIterator should" - {
      "yield the wole tree in post-order" in {
        val nodes = tree.postOrderIterator.toList

        nodes should have length 17
        nodes(0) shouldBe TreeLeaf("0")
        nodes(1) shouldBe TreeLeaf("1")
        nodes(2) shouldBe TreeLeaf("2")
        nodes(3) shouldBe a[TreeBin]
        nodes(4) shouldBe a[TreeBin]
        nodes(5) shouldBe TreeLeaf("3")
        nodes(6) shouldBe a[TreeOpt]
        nodes(7) shouldBe TreeLeaf("4")
        nodes(8) shouldBe TreeLeaf("5")
        nodes(9) shouldBe a[TreeBin]
        nodes(10) shouldBe TreeLeaf("6")
        nodes(11) shouldBe TreeLeaf("7")
        nodes(12) shouldBe a[TreeOpt]
        nodes(13) shouldBe TreeLeaf("8")
        nodes(14) shouldBe a[TreeList]
        nodes(15) shouldBe a[TreeOpt]
        nodes(16) shouldBe a[TreeBin]
      }
    }

    "visit should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          val subTrees = new ListBuffer[TestTree]
          tree visit {
            case TreeOpt(Some(tree)) => subTrees += tree
          }
          subTrees.loneElement should matchPattern { case TreeList(_) => }
        }

        "2" in {
          val text = new StringBuilder
          tree visit {
            case TreeLeaf(string) => text append string
          }
          text.toString shouldBe "012345678"
        }
      }
    }

    "visitAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          val subTrees = new ListBuffer[TestTree]
          tree visitAll {
            case TreeOpt(Some(tree)) => subTrees += tree
          }
          subTrees should have length 2
          subTrees(0) should matchPattern { case TreeList(_) => }
          subTrees(1) shouldBe TreeLeaf("7")
        }

        "2" in {
          val text = new StringBuilder
          tree visitAll {
            case _: TreeBin       => text append "b"
            case TreeLeaf(string) => text append string
          }
          text.toString shouldBe "bb0b123b45678"
        }
      }
    }

    "collect should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          val subTrees = {
            tree collect {
              case TreeOpt(Some(tree)) => tree
            }
          }.toList
          subTrees.loneElement should matchPattern { case TreeList(_) => }
        }

        "2" in {
          val text = tree collect {
            case TreeLeaf(string) => string
          }
          text mkString "-" shouldBe "0-1-2-3-4-5-6-7-8"
        }
      }

      "return an empty iterable when nothing matched" in {
        val it = tree collect {
          case TreeLeaf("99") => ()
        }
        it shouldBe empty
      }
    }

    "collectAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          val subTrees = {
            tree collectAll {
              case TreeOpt(Some(tree)) => tree
            }
          }.toList
          subTrees should have length 2
          subTrees(0) should matchPattern { case TreeList(_) => }
          subTrees(1) shouldBe TreeLeaf("7")
        }

        "2" in {
          val text = tree collectAll {
            case _: TreeBin       => "b"
            case TreeLeaf(string) => string
          }
          text mkString "-" shouldBe "b-b-0-b-1-2-3-b-4-5-6-7-8"
        }
      }

      "return an empty iterable when nothing matched" in {
        val it = tree collectAll {
          case TreeLeaf("99") => ()
        }
        it shouldBe empty
      }
    }

    "flatCollect should" - {
      "walk nodes in pre-order and stop when matching nodes are found" - {
        "1" in {
          val subTrees = {
            tree flatCollect {
              case TreeOpt(Some(tree)) => List(tree, "silly")
            }
          }.toList
          subTrees should have length 2
          subTrees(0) should matchPattern { case TreeList(_) => }
          subTrees(1) shouldBe "silly"
        }

        "2" in {
          val text = tree flatCollect {
            case TreeLeaf(string) => Seq(string, 3.14)
          }
          text mkString "-" shouldBe "0-3.14-1-3.14-2-3.14-3-3.14-4-3.14-5-3.14-6-3.14-7-3.14-8-3.14"
        }
      }

      "return an empty iterable when nothing matched" in {
        val it = tree flatCollect {
          case TreeLeaf("99") => List((), "a")
        }
        it shouldBe empty
      }
    }

    "flatCollectAll should" - {
      "keep going even if matching nodes are found" - {
        "1" in {
          val subTrees = {
            tree flatCollectAll {
              case TreeOpt(Some(tree)) => List(tree, "silly")
            }
          }.toList
          subTrees should have length 4
          subTrees(0) should matchPattern { case TreeList(_) => }
          subTrees(1) shouldBe "silly"
          subTrees(2) shouldBe TreeLeaf("7")
          subTrees(3) shouldBe "silly"
        }

        "2" in {
          val text = tree flatCollectAll {
            case _: TreeBin       => Seq("b", 2.72)
            case TreeLeaf(string) => Seq(string, 3.14)
          }
          text mkString "-" shouldBe {
            "b-2.72-b-2.72-0-3.14-b-2.72-1-3.14-2-3.14-3-3.14-b-2.72-4-3.14-5-3.14-6-3.14-7-3.14-8-3.14"
          }
        }
      }

      "return an empty iterable when nothing matched" in {
        val it = tree flatCollect {
          case TreeLeaf("99") => List((), "a")
        }
        it shouldBe empty
      }
    }

    "collectFirst should" - {
      "walk nodes in pre-order and stop at the very first match" - {
        "1" in {
          val subTree = {
            tree collectFirst {
              case TreeOpt(Some(tree)) => tree
            }
          }
          subTree.value should matchPattern { case TreeList(_) => }
        }

        "2" in {
          val text = tree collectFirst {
            case TreeLeaf(string) => string
          }
          text.value shouldBe "0"
        }
      }

      "return None when nothing matched" in {
        val opt = tree collectFirst {
          case TreeLeaf("99") => ()
        }
        opt shouldBe None
      }
    }

    "forall should" - {
      "walk nodes in pre-order and stop when matching nodes are found" in {
        val result = tree forall {
          case TreeOpt(opt) => opt.isDefined
        }
        result shouldBe true
      }

      "return true when nothing matched" in {
        val result = tree forall {
          case TreeLeaf("99") => false
        }
        result shouldBe true
      }
    }

    "forallAll should" - {
      "walk nodes in any order and keep going, checking all nodes" in {
        val result = tree forallAll {
          case TreeOpt(opt) => opt.isDefined
        }
        result shouldBe false
      }

      "return true when nothing matched" in {
        val result = tree forallAll {
          case TreeLeaf("99") => false
        }
        result shouldBe true
      }
    }

    "exists should" - {
      "walk nodes in pre-order and stop when matching nodes are found" in {
        val result = tree exists {
          case TreeOpt(opt) => !opt.isDefined
        }
        result shouldBe false
      }

      "return false when nothing matched" in {
        val result = tree exists {
          case TreeLeaf("99") => true
        }
        result shouldBe false
      }
    }

    "existsAll should" - {
      "walk nodes in any order and keep going, checking all nodes" in {
        val result = tree existsAll {
          case TreeOpt(opt) => !opt.isDefined
        }
        result shouldBe true
      }

      "return false when nothing matched" in {
        val result = tree existsAll {
          case TreeLeaf("99") => true
        }
        result shouldBe false
      }
    }

  }

}
