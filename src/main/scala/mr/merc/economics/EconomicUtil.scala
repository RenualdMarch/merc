package mr.merc.economics

object EconomicUtil {

  def subtractMapFromMap[K, V](from: Map[K, V], what: Map[K, V])(implicit num: Numeric[V]):Map[K, V] = {
    // intersecting keys
    val first = from.map{case (p, c) => p -> num.minus(c, what.getOrElse(p, num.zero))}
    val absentKeys = what.keySet -- first.keySet
    first ++ absentKeys.map(k => k -> num.negate(what(k))).toMap
  }
}
