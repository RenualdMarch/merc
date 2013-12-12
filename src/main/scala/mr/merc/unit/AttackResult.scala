package mr.merc.unit

// TODO remove defenders attack ?
// TODO make damage equal zero when success is false
case class AttackResult(isAttackerAttackingThisRound: Boolean, attacker: Soldier, defender: Soldier, attackersAttack: Attack, success: Boolean, damage: Int, drained: Int) {
  def attackIndex = attacker.soldierType.attacks.indexOf(attackersAttack)

  def applyDamage() = if (success) {
    defender.hp -= damage
    if (attackersAttack.attributes.contains(Drain)) {
      attacker.hp += drained
    }

    if (attackersAttack.attributes.contains(Slow)) {
      defender.addState(Slowed)
    }

    if (attackersAttack.attributes.contains(Poison)) {
      defender.addState(Poisoned)
    }
  }
}