////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2020 Argon Design Ltd. All rights reserved.
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.antlr

import com.argondesign.alogic.antlr.AntlrConverters._
import com.argondesign.alogic.core.Loc
import org.antlr.v4.runtime.ParserRuleContext

class AlogicParserRuleContext(parent: ParserRuleContext, invokingStateNumber: Int)
    extends ParserRuleContext(parent, invokingStateNumber) {

  // Note: Sadly I can't seem to find a way to call the no args constructor
  // of the superclass ParserRuleContext here, on the current version of Antlr
  // this does the same thing. Need to verify this when updating Antlr.
  def this() = this(null, -1)

  def txt: String = getText

  lazy val loc: Loc = {
    val sLoc = start.loc
    val eLoc = stop.loc
    Loc(sLoc.file, sLoc.line, sLoc.source, sLoc.start, eLoc.end, sLoc.point, sLoc.trueFileOpt)
  }

}
