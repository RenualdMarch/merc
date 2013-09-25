package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.map.hex.Direction
import mr.merc.map.hex._

abstract class SoldierViewState extends SpriteState {

}

abstract class SoldierViewAttackState(val success:Boolean, val direction:Direction, val number:Int) extends SoldierViewState

object SoldierViewAttackState {
  private val allStates = Set(FirstAttackNSuccState, FirstAttackNFailState, SecondAttackNSuccState, SecondAttackNFailState,
      FirstAttackNESuccState, FirstAttackNEFailState, SecondAttacNEkSuccState, SecondAttacNEkFailState,
      FirstAttackSESuccState, FirstAttackSEFailState, SecondAttackSESuccState, SecondAttackSEFailState,
      FirstAttackSSuccState, FirstAttackSFailState, SecondAttackSSuccState, SecondAttackSFailState,
      FirstAttackSWSuccState, FirstAttackSWFailState, SecondAttackSWSuccState, SecondAttackSWFailState,
      FirstAttackNWSuccState, FirstAttackNWFailState, SecondAttackNWSuccState, SecondAttackNWFailState).map(m => ((m.success, m.direction, m.number)-> m)).toMap
      
  def all = allStates.values.toList
      
  def apply(success:Boolean, dir:Direction, number:Int) = allStates(success, dir, number)    
}

object FirstAttackNSuccState extends SoldierViewAttackState(true, N, 0)
object FirstAttackNFailState extends SoldierViewAttackState(false, N, 0)
object SecondAttackNSuccState extends SoldierViewAttackState(true, N, 1)
object SecondAttackNFailState extends SoldierViewAttackState(false, N, 1)

object FirstAttackNESuccState extends SoldierViewAttackState(true, NE, 0)
object FirstAttackNEFailState extends SoldierViewAttackState(false, NE, 0)
object SecondAttacNEkSuccState extends SoldierViewAttackState(true, NE, 1)
object SecondAttacNEkFailState extends SoldierViewAttackState(false, NE, 1)

object FirstAttackSESuccState extends SoldierViewAttackState(true, SE, 0)
object FirstAttackSEFailState extends SoldierViewAttackState(false, SE, 0)
object SecondAttackSESuccState extends SoldierViewAttackState(true, SE, 1)
object SecondAttackSEFailState extends SoldierViewAttackState(false, SE, 1)

object FirstAttackSSuccState extends SoldierViewAttackState(true, S, 0)
object FirstAttackSFailState extends SoldierViewAttackState(false, S, 0)
object SecondAttackSSuccState extends SoldierViewAttackState(true, S, 1)
object SecondAttackSFailState extends SoldierViewAttackState(false, S, 1)

object FirstAttackSWSuccState extends SoldierViewAttackState(true, SW, 0)
object FirstAttackSWFailState extends SoldierViewAttackState(false, SW, 0)
object SecondAttackSWSuccState extends SoldierViewAttackState(true, SW, 1)
object SecondAttackSWFailState extends SoldierViewAttackState(false, SW, 1)

object FirstAttackNWSuccState extends SoldierViewAttackState(true, NW, 0)
object FirstAttackNWFailState extends SoldierViewAttackState(false, NW, 0)
object SecondAttackNWSuccState extends SoldierViewAttackState(true, NW, 1)
object SecondAttackNWFailState extends SoldierViewAttackState(false, NW, 1)



object DefenceState extends SoldierViewState
object IdleState extends SoldierViewState
object MoveState extends SoldierViewState
object StandState extends SoldierViewState
object DeathState extends SoldierViewState
object NoState extends SoldierViewState