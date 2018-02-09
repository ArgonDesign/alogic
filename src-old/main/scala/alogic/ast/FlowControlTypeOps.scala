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

package alogic.ast

trait FlowControlTypeOps extends FlowControlTypePrettyPrintOps { this: FlowControlType =>

  // Does this sync type contain a valid line?
  def hasValid = this match {
    case FlowControlTypeNone => false
    case _                   => true
  }

  // Does this sync type contain a ready line?
  def hasReady = this match {
    case FlowControlTypeReady => true
    case _                    => false
  }

  // Does this sync type contain an accept line?
  def hasAccept = this match {
    case FlowControlTypeAccept => true
    case _                     => false
  }
}
