////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Peter de Rivaz/Geza Lore
//
// DESCRIPTION:
//
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

// This file contains some useful functions for manipulating the abstract syntax tree

package alogic.ast

import alogic.Message
import alogic.Str
import alogic.StrList
import alogic.StrTree

object AstOps {
  def ExtractName(tree: Node): String = tree match {
    case DottedName(ns)    => ns.head
    case ArrayLookup(a, _) => ExtractName(a)
    case _                 => "Unknown"
  }

  // Does this sync type contain a valid line?
  def HasValid(s: FlowControlType): Boolean = s match {
    case FlowControlTypeNone => false
    case _                   => true
  }

  // Does this sync type contain a ready line?
  def HasReady(s: FlowControlType): Boolean = s match {
    case FlowControlTypeReady => true
    case _                    => false
  }

  // Does this sync type contain an accept line?
  def HasAccept(s: FlowControlType): Boolean = s match {
    case FlowControlTypeAccept => true
    case _                     => false
  }

  // Call VisitType to get callback for each node
  def VisitType(typ: Type, name: StrTree)(callback: (Type, StrTree) => Unit): Unit = {

    def visit(typ: Type, name: StrTree): Unit = {
      callback(typ, name)
      typ match {
        case Struct(_, fields) => for ((n, t) <- fields) {
          visit(t, StrList(name :: Str("_") :: Str(n) :: Nil))
        }
        case _ => ()
      }
    }
    visit(typ, name)
  }
}
