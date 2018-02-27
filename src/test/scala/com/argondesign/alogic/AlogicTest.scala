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
// Base traits for compiler tests
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic

import org.scalatest.Inside
import org.scalatest.Inspectors
import org.scalatest.LoneElement
import org.scalatest.Matchers
import org.scalatest.OneInstancePerTest
import org.scalatest.Suite

trait AlogicTest extends Matchers with LoneElement with Inspectors with Inside with OneInstancePerTest {
  this: Suite =>
}
