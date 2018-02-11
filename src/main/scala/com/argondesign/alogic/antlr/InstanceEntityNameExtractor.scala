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
// Given an Alogic parse tree, return a list of entity names that are
// instantiated in this tree
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import scala.collection.mutable

import com.argondesign.alogic.antlr.AlogicParser.InstanceContext
import com.argondesign.alogic.antlr.AntlrConverters._

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTreeWalker

object InstanceEntityNameExtractor {

  def apply(parseTree: ParserRuleContext): List[String] = {

    val names = mutable.Set[String]()

    object Extractor extends AlogicParserBaseListener {
      override def enterInstance(ctx: InstanceContext): Unit = {
        names += ctx.IDENTIFIER(1).text
      }
    }

    val walker = new ParseTreeWalker
    walker.walk(Extractor, parseTree)

    names.toList
  }

}
