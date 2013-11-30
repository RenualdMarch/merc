package mr.merc.unit

object SoldierTypeAttribute {
  private val list = List[SoldierTypeAttribute](Cures, Heals4, Heals8, Regenerates, Skirmisher, Steadfast)
  private val map = list map (a => a.toString() -> a) toMap
  def apply(name:String) = map(name)
}

sealed trait SoldierTypeAttribute {

}

case object Cures extends SoldierTypeAttribute
    // A unit which can cure an ally of poison, although the ally 
	// will receive no additional healing on the turn it is cured of the poison. 
case object Heals4 extends SoldierTypeAttribute
    // Allows the unit to heal adjacent friendly units at the 
	// beginning of each turn. A unit cared for by this healer may heal 
	// up to 4 HP per turn, or stop poison from taking effect for that
	// turn. A poisoned unit cannot be cured of its poison by a healer, 
	// and must seek the care of a village or a unit that can cure. 
case object Heals8 extends SoldierTypeAttribute
    // This unit combines herbal remedies with magic to heal units 
    // more quickly than is normally possible on the battlefield. A unit 
    // cared for by this healer may heal up to 8 HP per turn, or stop 
    // poison from taking effect for that turn. A poisoned unit cannot 
    // be cured of its poison by a healer, and must seek the care of a 
    // village or a unit that can cure. 

case object Regenerates extends SoldierTypeAttribute
    // This unit will heal itself 8HP per turn. If it is poisoned, 
	// it will remove the poison instead of healing. 
case object Skirmisher extends SoldierTypeAttribute
    // This unit is skilled in moving past enemies quickly, and ignores 
	// all enemy Zones of Control. 
case object Steadfast extends SoldierTypeAttribute
    // This units resistances are doubled, up to a maximum of 50%, 
	// when defending. Vulnerabilities are not affected. 

// TODO Think about this
// case object Leadership extends SoldierTypeAttribute
    // This unit can lead friendly units that are next to it, 
    // making them fight better. Adjacent friendly units of lower 
    // level will do more damage in battle. When a unit adjacent to, 
    // of a lower level than, and on the same side as a unit with 
    // Leadership engages in combat, its attacks do 25% more damage 
    // times the difference in their levels.
// case object Ambush extends SoldierTypeAttribute
    // This unit can hide in forest, and remain undetected by its enemies. 
    // Enemy units cannot see this unit while it is in forest, except if they 
	// have units next to it. Any enemy unit that first discovers this unit 
	// immediately loses all its remaining movement. 
// case object Concealment extends SoldierTypeAttribute
    // This unit can hide in villages (with the exception of water villages), 
    // and remain undetected by its enemies, except by those standing next to 
    // it. Enemy units can not see this unit while it is in a village, except 
    // if they have units next to it. Any enemy unit that first 
    // discovers this unit immediately loses all its remaining movement. 