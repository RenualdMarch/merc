package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.map.hex.Direction
import mr.merc.map.hex._

trait SoldierViewState extends SpriteState {

}

case class SoldierViewAttackState(success:Boolean, direction:Direction, number:Int) extends SoldierViewState

object DefenceState extends SoldierViewState
object IdleState extends SoldierViewState
object MoveState extends SoldierViewState
object StandState extends SoldierViewState
object DeathState extends SoldierViewState
object NoState extends SoldierViewState