////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
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

  def ExtractName(tree: Declaration): String = tree match {
    case VarDeclaration(_, id, _)          => id
    case ArrayDeclaration(_, id, _)        => id
    case ConstDeclaration(_, id, _)        => id
    case ParamDeclaration(_, id, _)        => id
    case VerilogVarDeclaration(_, id)      => id
    case VerilogArrayDeclaration(_, id, _) => id
    case OutDeclaration(_, _, name)        => name
    case InDeclaration(_, _, name)         => name
    case _                                 => Message.ice("unreachable")
  }

  // Does this sync type contain a valid line?
  def HasValid(s: SyncType): Boolean = s match {
    case Wire => false
    case _    => true
  }

  // Does this sync type contain a ready line?
  def HasReady(s: SyncType): Boolean = s match {
    case SyncReadyBubble => true
    case SyncReady       => true
    case _               => false
  }

  // Does this sync type contain an accept line?
  def HasAccept(s: SyncType): Boolean = s match {
    case SyncAccept     => true
    case WireSyncAccept => true
    case _              => false
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
