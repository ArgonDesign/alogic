////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017-2018 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// Module: Alogic Compiler
// Author: Geza Lore
//
// DESCRIPTION:
//
// Trait providing the 'followedBy' word
//
// The 'followedBy' word can be used to execute some side-effecting action
// after computing some value. The canonical example being:
//  def transform(tree:Tree): Tree = {
//    ...
//    newTree
//  } followedBy {
//    popScope()
//  }
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

// For importing with FollowedBy._
object FollowedBy {
  implicit final class FollowedByImpl[T](val value: T) extends AnyVal {
    def followedBy(block: => Unit): T = { block; value }
  }
}

// For mixing into classes
trait FollowedBy {
  import FollowedBy.FollowedByImpl
  implicit final def any2FollowedByImpl[T](value: T): FollowedByImpl[T] = new FollowedByImpl(value)
}
