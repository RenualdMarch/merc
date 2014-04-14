package mr.merc.world

import mr.merc.map.world.Province

class War(val attacker: Country, val defender: Country, val target: Province) {
  var provincesUnderAttackersControl = attacker.provinces.toSet
  var provincesUnderDefendersControl = defender.provinces.toSet

}