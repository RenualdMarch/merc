package mr.merc.util

import pureconfig.{BasicReaders, CamelCase, ConfigFieldMapping}
import pureconfig.generic.ProductHint
import scalafx.beans.binding.{Bindings, ObjectBinding, StringBinding}
import scalafx.beans.property.ReadOnlyObjectProperty
import scalafx.beans.value.ObservableValue
import scalafx.scene.paint.Color

import scala.util.Random

object MercUtils {
  implicit class RandomElement[T](trav: Traversable[T]) {
    def randomElement(): T = {
      if (trav.isEmpty) sys.error("traversable is empty!")
      else trav.toIndexedSeq(Random.nextInt(trav.size))
    }
  }

  implicit class Randomize(seq:Seq[Int]) {
    def shuffle(n: Int):Seq[Int] = {
      (0 until n).foldLeft(seq){case (s, _)=> makeOneShuffle(s)}
    }

    private def makeOneShuffle(s:Seq[Int]):Seq[Int] = {
      val from = Random.nextInt(s.size)
      val to = Random.nextInt(s.size)
      if (from != to && s(from) > 1) {
        s.updated(from, s(from) - 1).updated(to, s(to) + 1)
      } else s
    }
  }

  def printTime[T](message:String)(f: => T): T = {
    val before = System.currentTimeMillis()
    val res = f
    println(message + " " + (System.currentTimeMillis() - before).toString)
    res
  }

  def stablePseudoRandomIndex(x: Int, y: Int, size: Int): Int = {
    (x * 19 + y * 17) % size
  }

  def colorToStyle(color: Color):String = {
    s"rgb(${(color.red * 255).toInt},${(color.green * 255).toInt},${(color.blue * 255).toInt})"
  }

  object ConfigConvertProtocol {
    implicit def camelCaseHint[T]: ProductHint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
  }

  implicit class PropertyBindingMap[T](property: ObservableValue[T, T]) {
    def map[K](f: T => K, default:K = null):ObjectBinding[K] = {
      Bindings.createObjectBinding(() => {
        Option(property.value).map(f).getOrElse(default)
      }, property)
    }
  }
}