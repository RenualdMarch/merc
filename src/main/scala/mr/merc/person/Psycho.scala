package mr.merc.person

import scala.util.Random

class Psycho(val socioType: SocioType = SocioType.randomType) {
  import Math._

  var enemies: Set[Person] = Set()
  var friends: Set[Person] = Set()
  val morale = Morale(0.5)

  def refreshRelationships(current: Person, other: Person): Unit = {
    require(current.psycho == this)
    if (!enemies.contains(other) && !friends.contains(other)) {
      if (isConflict(other)) {
        enemies += other
        other.psycho.enemies += current
      } else if (isFriendship(other)) {
        friends += other
        other.psycho.friends += current
      }
    }
  }

  def isConflict(other: Person): Boolean = {
    val r = other.psycho.socioType.relations(socioType).relations
    val q = r + Random.nextDouble
    if (q > 0) false
    else pow(abs(q), 2 - morale.moraleLevel) > Random.nextDouble()
  }

  def isFriendship(other: Person): Boolean = {
    val r = other.psycho.socioType.relations(socioType).relations
    val q = r - Random.nextDouble
    if (q < 0) false
    else pow(abs(q), 1 + morale.moraleLevel) > Random.nextDouble()
  }
}

object SocioType {
  def randomType = SocioType(Random.nextBoolean(), Random.nextBoolean(), Random.nextBoolean(), Random.nextBoolean())

  sealed abstract class Relations(val relations: Double) // -1 to 1
  object Identity extends Relations(0.6)
  object Dual extends Relations(1)
  object Activation extends Relations(0.9)
  object Mirror extends Relations(0.5)
  object Ordering extends Relations(0)
  object Revision extends Relations(-0.9)
  object Mirage extends Relations(0.5)
  object Super extends Relations(-0.8)
  object Opposite extends Relations(0)
  object LikeIdentity extends Relations(-0.2)
  object Conflict extends Relations(-1)
  object Relative extends Relations(0.2)
  object Business extends Relations(0.4)
  object HalfDual extends Relations(0.7)
}

case class SocioType(isExtra: Boolean, isIntuition: Boolean, isThinker: Boolean, isJudging: Boolean) {
  import SocioType._
  override def toString: String = {
    val f = if (isExtra) "E" else "I"
    val s = if (isIntuition) "N" else "S"
    val t = if (isThinker) "T" else "F"
    val l = if (isJudging) "J" else "P"
    s"$f$s$t$l"
  }

  def relations(o: SocioType): Relations = {
    (isExtra ^ o.isExtra, isIntuition ^ o.isIntuition, isThinker ^ o.isThinker, isJudging ^ o.isJudging, isJudging) match {
      case (true, true, true, true, _) => Conflict
      case (true, true, false, true, _) => Revision
      case (true, false, true, true, _) => Revision
      case (true, false, false, true, _) => Mirror
      case (false, true, true, true, _) => Activation
      case (false, true, false, true, _) => Ordering
      case (false, false, true, true, _) => Ordering
      case (false, false, false, true, _) => LikeIdentity
      case (true, true, true, false, _) => Dual
      case (true, true, false, false, true) => Mirage
      case (true, true, false, false, false) => HalfDual
      case (true, false, true, false, true) => HalfDual
      case (true, false, true, false, false) => Mirage
      case (true, false, false, false, _) => Opposite
      case (false, true, true, false, _) => Super
      case (false, true, false, false, true) => Relative
      case (false, true, false, false, false) => Business
      case (false, false, true, false, true) => Business
      case (false, false, true, false, false) => Relative
      case (false, false, false, false, _) => Identity
    }
  }


}

case class Morale(var moraleLevel: Double) {
  require(moraleLevel >= 0 && moraleLevel <= 1, s"Incorrect morale level: $moraleLevel")

  private def changeMorale(q: Double): Unit = {
    // TODO change formula
    val newMorale = (moraleLevel + q) / (1 + moraleLevel * q)
    moraleLevel = (moraleLevel * 3 + newMorale) / 4
    if (moraleLevel < 0) moraleLevel = 0
  }

  def win(chancesOfVictory: Double): Unit = {
    changeMorale(1 - chancesOfVictory)
  }

  def lost(chancesOfVictory: Double): Unit = {
    changeMorale(-chancesOfVictory)
  }
}