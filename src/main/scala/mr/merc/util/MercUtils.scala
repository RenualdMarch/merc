package mr.merc.util

import scala.util.Random

object MercUtils {
  implicit def randomElement[T](trav: Traversable[T]) = new {
    def randomElement(): T = {
      if (trav.isEmpty) sys.error("traversable is empty!")
      else trav.toIndexedSeq(Random.nextInt(trav.size))
    }
  }
}