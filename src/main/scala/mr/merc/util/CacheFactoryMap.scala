package mr.merc.util

import scala.collection.mutable

class CacheFactoryMap[P, R](f:P => R) extends Function[P, R]{
  private val m:mutable.Map[P, R] = mutable.Map()

  def apply(p:P):R = {
    if (!m.contains(p)) {
      m += p -> f(p)
    }

    m(p)
  }
}

object CacheFactoryMap {
  def memo[I, O](f: I => O): I => O = new CacheFactoryMap(f)
}
