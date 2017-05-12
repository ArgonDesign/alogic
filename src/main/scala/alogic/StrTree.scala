package alogic

// An efficient tree string builder (hopefully...) 
// The idea is that we construct an immutable tree representation of strings
// With a fast toString function
sealed trait StrTree
case class Str(name: String) extends StrTree
case class StrList(names: List[StrTree]) extends StrTree
case class StrCommaList(names: List[StrTree]) extends StrTree

object MakeString {
  def apply(tree: StrTree): String = {
    val b = StringBuilder.newBuilder
    
    def go(tree: StrTree): Unit = tree match {
      case Str(name) => b.append(name)
      case StrList(names) => names foreach go
      case StrCommaList(names) => {
        go(names.head)
        for ( n <- names.tail ) {
          b.append(",")
          go(n)
        }
      }
    }
    
    go(tree)
    b.toString
  }
}
