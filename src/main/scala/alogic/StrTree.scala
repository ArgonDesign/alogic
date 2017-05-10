package alogic

// An efficient tree string builder (hopefully...) 
// The idea is that we construct an immutable tree representation of strings
// With a fast toString function
sealed trait StrTree
case class Str(name: String) extends StrTree
case class StrList(names: List[StrTree]) extends StrTree
case class StrCommaList(names: List[StrTree]) extends StrTree

object MakeString {
  def go(tree: StrTree, b: StringBuilder): Unit = tree match {
    case Str(name) => b.append(name)
    case StrList(names) => for ( n <- names ) go(n,b)
    case StrCommaList(names) => {
      go(names.head,b)
      for ( n <- names.tail ) {
        b.append(",")
        go(n,b)
      }
    }
  }
  def apply(tree: StrTree): String = {
    val b = StringBuilder.newBuilder
    go(tree,b)
    b.toString()
  }
}
