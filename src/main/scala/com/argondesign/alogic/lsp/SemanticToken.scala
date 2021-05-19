////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2017-2021 Argon Design Ltd. All rights reserved.
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
//
// DESCRIPTION:
// Semantic token as defined by
// https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.lsp

import com.argondesign.alogic.core.Loc

object SemanticTokenType extends Enumeration {
  type Type = Value
  val Namespace = Value("namespace")
  val Type = Value("type")
  val Class = Value("class")
  val Enum = Value("enum")
  val Interface = Value("interface")
  val Struct = Value("struct")
  val TypeParameter = Value("typeParameter")
  val Parameter = Value("parameter")
  val Variable = Value("variable")
  val Property = Value("property")
  val EnumMember = Value("enumMember")
  val Event = Value("event")
  val Function = Value("function")
  val Method = Value("method")
  val Macro = Value("macro")
  val Keyword = Value("keyword")
  val Modifier = Value("modifier")
  val Comment = Value("comment")
  val String = Value("string")
  val Number = Value("number")
  val Regexp = Value("regexp")
  val Operator = Value("operator")
}

object SemanticTokenModifier extends Enumeration {
  type Type = Value
  val Declaration = Value("declaration")
  val Definition = Value("definition")
  val Readonly = Value("readonly")
  val Static = Value("static")
  val Deprecated = Value("deprecated")
  val Abstract = Value("abstract")
  val Async = Value("async")
  val Modification = Value("modification")
  val Documentation = Value("documentation")
  val DefaultLibrary = Value("defaultLibrary")
}

case class SemanticToken(
    loc: Loc,
    typ: SemanticTokenType.Type,
    typeModifiers: Seq[SemanticTokenModifier.Type] = Nil) {

  def getEncodedModifiers(): Int = typeModifiers.foldLeft(0)((a, i) => a + (1 << i.id))

  def startPosInLine(): Int = {
    val startLineOffset = loc.source.offsetFor(loc.source.lineFor(loc.start))
    loc.start - startLineOffset
  }

}
