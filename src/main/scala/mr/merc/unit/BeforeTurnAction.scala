package mr.merc.unit

sealed trait BeforeTurnAction {
  def action():Unit
  val source:Soldier
}

case class CureSoldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = target.removeState(Poisoned)
}

case class Heal4Soldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = target.hp += 4
}

case class Heal8Soldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = target.hp += 8
}

