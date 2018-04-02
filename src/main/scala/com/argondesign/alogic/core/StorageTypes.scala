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

  abstract sealed trait StorageSlice

  case object StorageSliceFwd extends StorageSlice
  case object StorageSliceBwd extends StorageSlice
  case object StorageSliceBubble extends StorageSlice

  abstract sealed trait StorageType

  case object StorageTypeDefault extends StorageType
  case object StorageTypeWire extends StorageType
  case object StorageTypeReg extends StorageType
  case class StorageTypeSlices(kinds: List[StorageSlice]) extends StorageType

}
