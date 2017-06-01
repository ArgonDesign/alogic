package alogic

import scala.collection.mutable

// Generic Cache
abstract class Cache[K, V] {

  // Register cache in the list of all caches
  Cache.register(this)

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
  def apply(key: Key)(construct: => Value): Value = synchronized {
    storage.getOrElseUpdate(index(key), construct)
  }

  // Clear cache
  def clear() = storage.clear()

}

// Simple cache where the Tags are the Keys,
// and the index is the identity function
class SimpleCache[K, V] extends Cache[K, V] {

  // Tags are same as the keys
  override type Tag = K

  // The index function is the identity function
  override def index(key: Key): Tag = key

}

// Cache object to keep track of all cache instances for maintenance
object Cache {

  // Note this is a Map using weak references, so items will
  // be removed if a key is no longer strongly referenced
  private val instances = mutable.WeakHashMap[Cache[_, _], Unit]()

  // Register cache instance for tracking
  private def register(cache: Cache[_, _]) = synchronized {
    instances(cache) = ()
  }

  // Clear all caches
  def clearAll() = synchronized {
    instances.keys foreach (_.clear)
  }

}
