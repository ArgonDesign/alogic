////////////////////////////////////////////////////////////////////////////////
// Argon Design Ltd. Project P8009 Alogic
// Copyright (c) 2017 Argon Design Ltd. All rights reserved.
//
// Module : Scala Alogic Compiler
// Author : Geza Lore
//
// DESCRIPTION:
//
// A generic functional cache
//
// This file is covered by the BSD (with attribution) license.
// See the LICENSE file for the precise wording of the license.
////////////////////////////////////////////////////////////////////////////////

package alogic

import scala.collection.mutable

// Generic Cache
// Client code must define:
// 1. type 'Tag', which is the unique identifier of cache entries,
// 2. member function 'index', which maps a Key to a Tag.
abstract class Cache[K, V] {

  // The type of the keys used to look up cache
  final type Key = K

  // The type of the values stored in the cache
  final type Value = V

  // The type of the cache Tags used to address storage
  protected[this] type Tag

  // Function mapping keys to tags
  protected[this] def index(key: Key): Tag

  // Actual storage
  private[this] val storage = mutable.Map[Tag, Value]();

  // Look up or update cache
  def apply(key: Key)(construct: => Value): Value = {
    val idx = index(key)
    synchronized {
      storage.getOrElseUpdate(idx, construct)
    }
  }

}

// Simple cache where:
// 1. type 'Tag' is the same as type 'Key',
// 2. The 'index' function is the identity.
class SimpleCache[K, V] extends Cache[K, V] {

  // Tags are same as the keys
  override type Tag = K

  // The index function is the identity function
  override def index(key: Key): Tag = key

}
