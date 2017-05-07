// This file contains some useful functions for manipulating the abstract syntax tree

package alogic

object ExtractName {
  def apply(tree:AlogicAST) : String = tree match {
    case DottedName(ns) => ns.head
    case ArrayLookup(a,_) => apply(a)
    case BinaryArrayLookup(a,_,_,_) => apply(a)
    case _ => "Unknown"
  }
}