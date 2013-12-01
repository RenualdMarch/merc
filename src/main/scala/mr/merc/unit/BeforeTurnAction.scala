package mr.merc.unit

object BeforeTurnAction {
  def filterActions(input:Set[BeforeTurnAction]):Set[BeforeTurnAction] = {
	input.groupBy(target).mapValues(_.toList.sortBy(priority).head).values.toSet
  }
  
  // lower means higher
  private def priority(act:BeforeTurnAction):Int = act match {
    case Regeneration(_) => 1
    case CureSoldier(_, _) => 2
    case PoisoningDamage(_) => 3
    case Heal8Soldier(_, _) => 4
    case Heal4Soldier(_, _) => 5
    case DoingNothingHeal2(_) => 6
  }
  
  private def target(act:BeforeTurnAction):Soldier = act match {
    case Regeneration(s) => s
    case CureSoldier(_, s) => s
    case PoisoningDamage(s) => s
    case Heal8Soldier(_, s) => s
    case Heal4Soldier(_, s) => s
    case DoingNothingHeal2(s) => s
  }
}

sealed trait BeforeTurnAction {
  def action():Unit
  val source:Soldier
}

case class CureSoldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = target.removeState(Poisoned)
}

case class Heal4Soldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = if (target.movedThisTurn) { 
    target.hp += 4
  } else {
    target.hp += 6
  }
}

case class Heal8Soldier(source:Soldier, target:Soldier) extends BeforeTurnAction {
  override def action() = if (target.movedThisTurn) { 
    target.hp += 8
  } else {
    target.hp += 10
  }
}

case class DoingNothingHeal2(source:Soldier) extends BeforeTurnAction {
  override def action() = source.hp += 2
}

case class Regeneration(source:Soldier) extends BeforeTurnAction {
  override def action() {
    if (source.state.contains(Poisoned)) {
      source.removeState(Poisoned)
    } else {
      Heal8Soldier(source, source).action()
    }
  }
}

case class PoisoningDamage(source:Soldier) extends BeforeTurnAction {
  override def action() = {
    source.hp -= 8
    if (source.hp <= 0) source.hp = 1
  }
}
