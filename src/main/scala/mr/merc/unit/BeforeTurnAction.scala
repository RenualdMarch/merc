package mr.merc.unit

sealed trait BeforeTurnAction {
  def action():Unit
}

case class CureSoldier(soldier:Soldier) extends BeforeTurnAction {
  override def action() = soldier.removeState(Poisoned)
}