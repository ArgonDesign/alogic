////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Provide the 'partialMatch' keyword.
//
// This can be used where a match needs to return an Option, but we do not
// want to specify all the None case explicitly:
//
// a match {
//   case Foo => Some(bar)
//   case _ => None
// }
//
// can be written as
//
// a partialMatch {
//   case Foo => bar
// }
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.util

import scala.language.implicitConversions

// For importing with PartialMatch._
object PartialMatch {

  implicit final class PartialMatchImpl[T](private val value: T) extends AnyVal {
    def partialMatch[U](pf: PartialFunction[T, U]): Option[U] = pf.lift(value)
  }

}

// For mixing into classes
trait PartialMatch {
  import PartialMatch.PartialMatchImpl

  implicit final def any2PartialMatchImpl[T](value: T): PartialMatchImpl[T] = {
    new PartialMatchImpl(value)
  }

}
