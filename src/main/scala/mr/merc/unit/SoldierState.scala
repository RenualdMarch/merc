package mr.merc.unit

sealed trait SoldierState {

}
// TODO write tests for states
case object Poisoned extends SoldierState
case object Slowed extends SoldierState