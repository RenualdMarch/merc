package mr.merc.economics

import mr.merc.economics.MapUtil.NumericOperations.MapWithOperations

object MapUtil {

  object NumericOperations {
    implicit class MapWithOperations[K, V](map: Map[K, V])(implicit  num: Numeric[V]) {

      def |+| (other: Map[K, V]):Map[K, V] = {
        val keysInMapOnly = map.keySet -- other.keySet
        val keysInOtherOnly = other.keySet -- map.keySet
        val intersectingKeys = map.keySet ++ other.keySet -- keysInMapOnly -- keysInOtherOnly
        val sumMap = intersectingKeys.map { k =>
          k -> num.plus(map(k), other(k))
        }.toMap
        map.filterKeys(keysInMapOnly.contains) ++ other.filterKeys(keysInOtherOnly.contains) ++ sumMap
      }

      def |-| (other: Map[K, V]): Map[K, V] = {
        // intersecting keys
        val first = map.map{case (p, c) => p -> num.minus(c, other.getOrElse(p, num.zero))}
        val absentKeys = other.keySet -- first.keySet
        first ++ absentKeys.map(k => k -> num.negate(other(k))).toMap
      }

      def |*| (q: V): Map[K, V] = {
        map.transform { case (_, v) =>
          num.times(v, q)
        }
      }
    }
  }

  object FloatOperations {
    implicit class MapWithFloatOperations[K, V](map: Map[K, V])(implicit  num: Fractional[V]) extends MapWithOperations[K, V](map){

      def scaleToSum(sum: V): Map[K, V] = {
        val currentSum = map.values.sum(num)
        val mult = num.div(sum, currentSum)
        map.transform { case (_, v) => num.times(v, mult) }
      }
    }
  }

}
