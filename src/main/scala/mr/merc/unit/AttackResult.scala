package mr.merc.unit

case class AttackResult(isAttackerAttackingThisRound:Boolean, attacker:Soldier, defender:Soldier, attackersAttack:Attack, defendersAttack:Option[Attack], success:Boolean, damage:Int, drained:Int) {
	def applyDamage() = if (success) {
	  defender.hp -= damage
	  if (attackersAttack.attributes.contains(Drain)) {
		  attacker.hp += drained
	  }
	}
}