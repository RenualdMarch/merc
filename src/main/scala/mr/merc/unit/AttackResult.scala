package mr.merc.unit

case class AttackResult(attacker:Soldier, defender:Soldier, attack:Attack, success:Boolean) {
	def applyDamage() = if (success) {
	  defender.damage(attack)
	}
}