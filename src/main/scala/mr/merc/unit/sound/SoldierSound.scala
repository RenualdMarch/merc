package mr.merc.unit.sound

sealed trait SoldierSound

object MovementSound extends SoldierSound
object PainSound extends SoldierSound
object DeathSound extends SoldierSound
case class AttackSound(number: Int, success: Boolean) extends SoldierSound

