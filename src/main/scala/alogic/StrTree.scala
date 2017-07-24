////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017 Argon Design Ltd.
// All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

// An efficient tree string builder (hopefully...)
// The idea is that we construct an immutable tree representation of strings
// With a fast toString function
sealed trait StrTree {
  override def toString: String = {
    val b = StringBuilder.newBuilder

    def reduce(names: Seq[StrTree], op: String, addBrackets: Boolean): Unit = {
      if (addBrackets)
        b.append("(")
      go(names.head)
      if (addBrackets)
        b.append(")")
      for (n <- names.tail) {
        b.append(op)
        if (addBrackets)
          b.append("(")
        go(n)
        if (addBrackets)
          b.append(")")
      }
    }

    def go(tree: StrTree): Unit = tree match {
      case Str(name)           => b.append(name)
      case StrList(Nil, _)     => ()
      case StrList(names, "")  => names foreach go
      case StrList(names, "+") => reduce(names, "+", true)
      case StrList(names, "*") => reduce(names, "*", true)
      case StrList(names, op)  => reduce(names, op, false)
    }

    go(this)
    b.toString
  }
}

case class Str(name: String) extends StrTree
case class StrList(names: Seq[StrTree], sep: String = "") extends StrTree
