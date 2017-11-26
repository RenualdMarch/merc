package mr.merc.util

import scala.collection.mutable

class CacheFactoryMap[P, R](f:P => R) {
  private val m:mutable.Map[P, R] = mutable.Map()

  def apply(p:P):R = {
    if (!m.contains(p)) {
      m += p -> f(p)
    }

    m(p)
  }
}
