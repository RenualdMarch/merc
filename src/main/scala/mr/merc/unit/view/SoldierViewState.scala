package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.map.hex.Direction
import mr.merc.map.hex._

trait SoldierViewState extends SpriteState {

}

case class SoldierViewAttackState(success:Boolean, direction:Direction, number:Int) extends SoldierViewState

case object DefenceState extends SoldierViewState
case object IdleState extends SoldierViewState
case object MoveState extends SoldierViewState
case object StandState extends SoldierViewState
case object DeathState extends SoldierViewState
case object NoState extends SoldierViewState