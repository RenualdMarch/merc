package mr.merc.unit

import scala.reflect.runtime.universe._

object AttackType {

  private val map = List(Blade, Impact, Cold, Pierce, Fire, Arcane).map(t => (t.name, t)).toMap
  def apply(name:String): AttackType = map(name)
}

sealed abstract class AttackType extends Product {
  val name:String = {
    val rootMirror = runtimeMirror(getClass.getClassLoader)
    val classSymbol = rootMirror.classSymbol(getClass)
    classSymbol.name.toString
  }
}

case object Blade extends AttackType
case object Impact extends AttackType
case object Cold extends AttackType
case object Pierce extends AttackType
case object Fire extends AttackType
case object Arcane extends AttackType