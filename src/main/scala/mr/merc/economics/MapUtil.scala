package mr.merc.economics

import cats.kernel.Semigroup
import mr.merc.economics.MapUtil.NumericOperations.{InnerMapWithOperations, MapWithOperations}

import scala.collection.mutable

object MapUtil {

  object NumericOperations {
    implicit class MapWithOperations[K, V](map: Map[K, V])(implicit  num: Numeric[V]) {

      def |+| (other: Map[K, V]):Map[K, V] = {
        val hashMap = mutable.Map[K, V]()
        map.foreach { case (k, v) =>
          hashMap.put(k, v)
        }
        other.foreach { case (k, v) =>
          val prev = hashMap.getOrElse(k, num.zero)
          hashMap.put(k, num.plus(prev, v))
        }
        hashMap.toMap
      }

      def |+| (other: (K, V)):Map[K, V] = {
        this |+| Map(other)
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

      def dot (other: Map[K, V]): V = {
        (this |*| other).values.sum
      }

      def |*|(other: Map[K, V]): Map[K, V] = {
        val intersectingKeys = this.map.keySet & other.keySet
        intersectingKeys.map { k =>
          k -> num.times(map(k), other(k))
        }.toMap
      }

      def sumValues:V = map.values.sum
    }

    implicit class CollectionMapWithOperations[K, V](iterable: Iterable[Map[K, V]])(implicit  num: Numeric[V]) {

      def sumAll:Map[K, V] = {
        val hashMap = mutable.Map[K, V]()
        for {
          map <- iterable
          (k, v) <- map
        } {
          val prev = hashMap.getOrElse(k, num.zero)
          hashMap.put(k, num.plus(v, prev))
        }
        hashMap.toMap
      }

    }

    implicit class InnerMapWithOperations[K1, K2, V](map: Map[K1, Map[K2, V]])(implicit  num: Numeric[V]) {
      def |++| (other:Map[K1, Map[K2, V]]):Map[K1, Map[K2, V]] = {
        val keysInMapOnly = map.keySet -- other.keySet
        val keysInOtherOnly = other.keySet -- map.keySet
        val intersectingKeys = map.keySet ++ other.keySet -- keysInMapOnly -- keysInOtherOnly
        val sumMap = intersectingKeys.map { k =>
          k -> (map(k) |+| other(k))
        }.toMap
        map.filterKeys(keysInMapOnly.contains) ++ other.filterKeys(keysInOtherOnly.contains) ++ sumMap
      }

      def |**|(q: V):Map[K1, Map[K2, V]] = {
        map.transform { case (_, v1) =>
          v1.transform { case (_, v2) =>
            num.times(v2, q)
          }
        }
      }
    }
  }

  object FloatOperations {

    implicit class MapWithFloatOperations[K, V](map: Map[K, V])(implicit num: Fractional[V]) extends MapWithOperations[K, V](map) {

      def scaleToSum(sum: V): Map[K, V] = {
        val currentSum = map.values.sum(num)
        val mult = num.div(sum, currentSum)
        map.transform { case (_, v) => num.times(v, mult) }
      }
    }

    implicit class InnerMapWithFloatOperations[K1, K2, V](map: Map[K1, Map[K2, V]])(implicit num: Fractional[V]) extends InnerMapWithOperations(map) {

    }
  }

}
