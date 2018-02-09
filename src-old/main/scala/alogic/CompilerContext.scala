////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// The CompilerContext holds all mutable state of the compiler.
// Throughout the compiler, the CompilerContext is held in a variable called
// 'cc', which is often passed as an implicit parameter.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

class CompilerContext
  extends Messaging
  with LocationRemapping {

  private[this] implicit val implicitThis: CompilerContext = this

  // The preprocessor uses caching, so it needs to live in cc
  lazy val preproc = new Preproc

}
