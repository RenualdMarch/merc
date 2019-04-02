package mr.merc.util

import javafx.beans.property.{ObjectPropertyBase, SimpleObjectProperty}
import javafx.beans.property.adapter.JavaBeanObjectProperty
import scalafx.beans.binding.Bindings
import scalafx.beans.property.{BooleanProperty, ObjectProperty}
import scalafx.beans.value.ObservableValue

import scala.collection.immutable


object FxPropertyUtils {

  implicit def objectPropertyToBooleanProperty(property: ObjectProperty[Boolean]):BooleanProperty = {
    val bProperty = new BooleanProperty()
    bProperty <== Bindings.createBooleanBinding(() => property.value, property)
    bProperty
  }

  class Default[+T](val default: T)

  trait LowerPriorityImplicits {
    // Stop AnyRefs from clashing with AnyVals
    implicit def defaultNull[A <: AnyRef]:Default[A] = new Default[A](null.asInstanceOf[A])
  }

  object Default extends LowerPriorityImplicits {
    implicit object DefaultDouble extends Default[Double](0.0)
    implicit object DefaultFloat extends Default[Float](0.0F)
    implicit object DefaultInt extends Default[Int](0)
    implicit object DefaultLong extends Default[Long](0L)
    implicit object DefaultShort extends Default[Short](0)
    implicit object DefaultByte extends Default[Byte](0)
    implicit object DefaultChar extends Default[Char]('\u0000')
    implicit object DefaultBoolean extends Default[Boolean](false)
    implicit object DefaultUnit extends Default[Unit](())

    implicit def defaultSeq[A]: Default[immutable.Seq[A]] = new Default[immutable.Seq[A]](immutable.Seq())
    implicit def defaultSet[A]: Default[Set[A]] = new Default[Set[A]](Set())
    implicit def defaultMap[A, B]: Default[Map[A, B]] = new Default[Map[A, B]](Map[A, B]())
    implicit def defaultOption[A]: Default[Option[A]] = new Default[Option[A]](None)

    def value[A](implicit value: Default[A]): A = value.default
  }

  implicit class PropertyBindingMap[T, J](property: ObservableValue[T, J]) {

    def map[K](f: T => K)(implicit d: Default[K]):ObjectProperty[K] = {
      val b = Bindings.createObjectBinding(() => {
        Option(property.value).map(f).getOrElse(d.default)
      }, property)
      val p = new ObjectProperty[K]()
      p <== b
      p
    }

    def forceInvalidation(): Unit = {
      property.delegate match {
        case bean:JavaBeanObjectProperty[_] => bean.fireValueChangedEvent()
        case simple:SimpleObjectProperty[_] =>
          val method = classOf[ObjectPropertyBase[_]].getDeclaredMethod("markInvalid")
          method.setAccessible(true)
          method.invoke(simple)
      }
    }
  }


}
