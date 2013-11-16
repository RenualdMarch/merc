package mr.merc.unit

case class AttackResult(isAttackerAttackingThisRound:Boolean, attacker:Soldier, defender:Soldier, attackersAttack:Attack, defendersAttack:Option[Attack], success:Boolean) {
	def applyDamage() = if (success) {
	  val damage = Attack.possibleAttackersDamage(isAttackerAttackingThisRound, attacker, defender, attackersAttack, defendersAttack)
	  defender.hp -= damage
	  if (defender.hp < 0) defender.hp = 0
	}
}