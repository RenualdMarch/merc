package mr.merc.unit

import mr.merc.local.Localization

object AttackAttribute {
  private val list = List[AttackAttribute](Berserk, Charge, Drain,
    Firststrike, Magical, Marksman, Poison, Slow)
  private val map = list map (a => a.toString() -> a) toMap
  def apply(name: String) = map(name)


  case object Berserk extends AttackAttribute {
    val localKey = "attackAttribute.berserk"
  }
  // Whether used offensively or defensively,
  // this attack presses the engagement until one of
  // the combatants is slain, or 30 rounds of attacks have occurred.
  case object Charge extends AttackAttribute {
    val localKey = "attackAttribute.charge"
  }
  // When used offensively, this attack deals double damage to the target.
  // It also causes this unit to take double damage from the targets counterattack.
  case object Drain extends AttackAttribute {
    val localKey = "attackAttribute.drain"
  }
  // This unit drains health from living units, healing itself for
  // half the amount of damage it deals (rounded down).
  case object Firststrike extends AttackAttribute {
    val localKey = "attackAttribute.firststrike"
  }
  // This unit always strikes first with this attack,
  // even if they are defending.
  case object Magical extends AttackAttribute {
    val localKey = "attackAttribute.magical"
  }
  // This attack always has a 70% chance to hit regardless
  // of the defensive ability of the unit being attacked.
  case object Marksman extends AttackAttribute {
    val localKey = "attackAttribute.marksman"
  }
  // When used offensively, this attack always has at least a 60%
  // chance to hit.
  case object Poison extends AttackAttribute {
    val localKey = "attackAttribute.poison"
  }
  // This attack poisons the target. Poisoned units lose 8 HP every turn
  // until they are cured or are reduced to 1 HP. Poison can not,
  // of itself, kill a unit.
  case object Slow extends AttackAttribute {
    val localKey = "attackAttribute.slow"
  }
  // This attack slows the target until it ends a turn.
  // Slow halves the damage caused by attacks and the movement
  // cost for a slowed unit is doubled. A unit that is slowed
  // will feature a snail icon in its sidebar information when it is selected.

  // NOT NEEDED NOW
  //case object Plague extends AttackAttribute
  // When a unit is killed by a Plague attack, that unit is replaced
  // with a Walking Corpse on the same side as the unit with the
  // Plague attack. This doesnt work on Undead or units in villages.
  //case object Petrify extends AttackAttribute
  // This attack petrifies the target, turning it to stone. Units that
  // have been petrified may not move or attack.


}

sealed trait AttackAttribute {
  val localKey: String
  def localizedName = Localization(localKey)
}

