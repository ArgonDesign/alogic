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
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

final class FollowedByWord[T](private val value: T) extends AnyVal {
  // The 'followedBy' word can be used to execute some side-effecting action
  // after computing some value. The canonical example being:
  //  def transform(tree:Tree): Tree = {
  //    ...
  //    newTree
  //  } followedBy {
  //    popScope()
  //  }
  final def followedBy(block: => Unit): T = { block; value }
}

// For mixing into classes
trait FollowedBy extends Any {
  final implicit def any2FollowedByWord[T](value: T): FollowedByWord[T] = new FollowedByWord(value)
}

// For importing with FollowedBy._
final object FollowedBy extends FollowedBy
