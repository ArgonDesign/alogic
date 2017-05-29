package alogic

import org.antlr.v4.runtime.ParserRuleContext
import scala.collection._
import scala.annotation.tailrec
import Antlr4Conversions._

// This class keeps track of the variables in our name space
// We can declare a new id, or lookup an exisiting one.
// Failures result in a warning.

final class Namespace {

  val variables = mutable.Set[String]() // Track all ids we are planning to use

  var namespaces: List[mutable.Map[String, String]] = Nil // Keep a stack of namespaces

  addNamespace() // Prepare default namespace for top level declarations

  def addNamespace() = { namespaces = mutable.Map[String, String]() :: namespaces }

  def removeNamespace() = { namespaces = namespaces.tail }

  def lookup(ctx: ParserRuleContext, name: String): String = lookup(ctx, name, namespaces)

  @tailrec def lookup(ctx: ParserRuleContext, name: String, ns: List[mutable.Map[String, String]]): String =
    if (ns.length <= 0) {
      Message.error(ctx.loc, s"Unknown identifier '$name'")
      s"Unknown_$name"
    } else {
      val n = ns.head
      if (n contains name)
        n(name)
      else
        lookup(ctx, name, ns.tail)
    }

  // Can only have one copy of each identifier in each namespace
  def insert(ctx: ParserRuleContext, name: String): String = {
    val n = namespaces.head
    if (n contains name)
      Message.error(ctx.loc, s"Multiple declarations of '$name'")
    insert(name)
  }

  def insert(name: String): String = {
    val n = namespaces.head
    var target = name
    while (variables contains target) {
      target += "Dup"
    }
    n(name) = target
    variables += target
    target
  }

}
