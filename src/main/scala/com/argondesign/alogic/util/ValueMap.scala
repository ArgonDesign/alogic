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
// Trait providing the 'valueMap' word
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

final class ValueMapWord[T](private val value: T) extends AnyVal {
  // The 'valueMap' word can be used apply a closure to a single value
  // e.g.: :
  //  1 valueMap {
  //    _ + 2
  //  } valueMap {
  //    _ * 3
  //  }
  // is the same as (1 + 2) * 3
  final def valueMap[R](f: T => R): R = f(value)
}

// For mixing into classes
trait ValueMap extends Any {
  final implicit def any2ValueMapWord[T](value: T): ValueMapWord[T] = new ValueMapWord(value)
}

// For importing with ValueMap._
final object ValueMap extends ValueMap
