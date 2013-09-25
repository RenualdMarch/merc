package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.map.hex.Directions

abstract class SoldierViewState extends SpriteState {

}

abstract class SoldierViewAttackState(val success:Boolean, val direction:Directions.Direction, val number:Int) extends SoldierViewState

object SoldierViewAttackState {
  private val allStates = Set(FirstAttackNSuccState, FirstAttackNFailState, SecondAttackNSuccState, SecondAttackNFailState,
      FirstAttackNESuccState, FirstAttackNEFailState, SecondAttacNEkSuccState, SecondAttacNEkFailState,
      FirstAttackSESuccState, FirstAttackSEFailState, SecondAttackSESuccState, SecondAttackSEFailState,
      FirstAttackSSuccState, FirstAttackSFailState, SecondAttackSSuccState, SecondAttackSFailState,
      FirstAttackSWSuccState, FirstAttackSWFailState, SecondAttackSWSuccState, SecondAttackSWFailState,
      FirstAttackNWSuccState, FirstAttackNWFailState, SecondAttackNWSuccState, SecondAttackNWFailState).map(m => ((m.success, m.direction, m.number)-> m)).toMap
      
  def all = allStates.values.toList
      
  def apply(success:Boolean, dir:Directions.Direction, number:Int) = allStates(success, dir, number)    
}

object FirstAttackNSuccState extends SoldierViewAttackState(true, Directions.N, 0)
object FirstAttackNFailState extends SoldierViewAttackState(false, Directions.N, 0)
object SecondAttackNSuccState extends SoldierViewAttackState(true, Directions.N, 1)
object SecondAttackNFailState extends SoldierViewAttackState(false, Directions.N, 1)

object FirstAttackNESuccState extends SoldierViewAttackState(true, Directions.NE, 0)
object FirstAttackNEFailState extends SoldierViewAttackState(false, Directions.NE, 0)
object SecondAttacNEkSuccState extends SoldierViewAttackState(true, Directions.NE, 1)
object SecondAttacNEkFailState extends SoldierViewAttackState(false, Directions.NE, 1)

object FirstAttackSESuccState extends SoldierViewAttackState(true, Directions.SE, 0)
object FirstAttackSEFailState extends SoldierViewAttackState(false, Directions.SE, 0)
object SecondAttackSESuccState extends SoldierViewAttackState(true, Directions.SE, 1)
object SecondAttackSEFailState extends SoldierViewAttackState(false, Directions.SE, 1)

object FirstAttackSSuccState extends SoldierViewAttackState(true, Directions.S, 0)
object FirstAttackSFailState extends SoldierViewAttackState(false, Directions.S, 0)
object SecondAttackSSuccState extends SoldierViewAttackState(true, Directions.S, 1)
object SecondAttackSFailState extends SoldierViewAttackState(false, Directions.S, 1)

object FirstAttackSWSuccState extends SoldierViewAttackState(true, Directions.SW, 0)
object FirstAttackSWFailState extends SoldierViewAttackState(false, Directions.SW, 0)
object SecondAttackSWSuccState extends SoldierViewAttackState(true, Directions.SW, 1)
object SecondAttackSWFailState extends SoldierViewAttackState(false, Directions.SW, 1)

object FirstAttackNWSuccState extends SoldierViewAttackState(true, Directions.NW, 0)
object FirstAttackNWFailState extends SoldierViewAttackState(false, Directions.NW, 0)
object SecondAttackNWSuccState extends SoldierViewAttackState(true, Directions.NW, 1)
object SecondAttackNWFailState extends SoldierViewAttackState(false, Directions.NW, 1)



object DefenceState extends SoldierViewState
object IdleState extends SoldierViewState
object MoveState extends SoldierViewState
object StandState extends SoldierViewState
object DeathState extends SoldierViewState
object NoState extends SoldierViewState