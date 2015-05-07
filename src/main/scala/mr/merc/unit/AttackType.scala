package mr.merc.unit

import scala.reflect.runtime.universe._

object AttackType {

  private val map = List(Blade, Impact, Cold, Pierce, Fire, Arcane).map(t => (t.name, t)).toMap
  def apply(name:String) = map(name)
}

sealed abstract class AttackType {
  val name:String = {
    val rootMirror = runtimeMirror(getClass.getClassLoader)
    val classSymbol = rootMirror.classSymbol(getClass)
    classSymbol.name.toString
  }
}

object Blade extends AttackType
object Impact extends AttackType
object Cold extends AttackType
object Pierce extends AttackType
object Fire extends AttackType
object Arcane extends AttackType