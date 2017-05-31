package alogic

// An efficient tree string builder (hopefully...)
// The idea is that we construct an immutable tree representation of strings
// With a fast toString function
sealed trait StrTree {
  override def toString: String = {
    val b = StringBuilder.newBuilder

    def reduce(names: List[StrTree], op: String, addBrackets: Boolean): Unit = {
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
      case StrList(names)      => names foreach go
      case StrCommaList(Nil)   => ()
      case StrCommaList(names) => reduce(names, ",", false)
      case StrProduct(names)   => reduce(names, "*", true)
      case StrSum(names)       => reduce(names, "+", true)
    }

    go(this)
    b.toString
  }
}

case class Str(name: String) extends StrTree
case class StrList(names: List[StrTree]) extends StrTree
case class StrCommaList(names: List[StrTree]) extends StrTree
case class StrProduct(names: List[StrTree]) extends StrTree
case class StrSum(names: List[StrTree]) extends StrTree
