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
// Representations of output port storage types
////////////////////////////////////////////////////////////////////////////////

package com.argondesign.alogic.core

object StorageTypes {

  abstract sealed trait StorageSliceType

  case object StorageSliceFwd extends StorageSliceType
  case object StorageSliceBwd extends StorageSliceType
  case object StorageSliceBubble extends StorageSliceType

  abstract sealed trait StorageType

  case object StorageTypeWire extends StorageType
  case object StorageTypeReg extends StorageType
  case class StorageTypeSlices(kinds: List[StorageSliceType]) extends StorageType

}
