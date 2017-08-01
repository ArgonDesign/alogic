package alogic.ast

trait DeclarationOps extends DeclarationPrettyPrintOps { this: Declaration =>
  val decltype: Type
  val id: String
}
