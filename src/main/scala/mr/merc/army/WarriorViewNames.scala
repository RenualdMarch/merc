package mr.merc.army

import mr.merc.army.WarriorType._

case class WarriorViewNames(possibleWarriors:Map[(WarriorType, WarriorCompetence), String]) {

}

object WarriorViewNames {

  // with wesnoth cavalry
  val LatinCulture = WarriorViewNames(Map(
    (HeavyBladeInfantry, Militia) -> "HumanLavinian-Legionnaire",
    (HeavyBladeInfantry, Professional) -> "HumanLavinian-Propugnator",
    (LightBladeInfantry, Militia) -> "HumanLavinian-LightInfantry",
    (LightBladeInfantry, Professional) -> "HumanLavinian-Rorarius",
    (Archer, Militia) -> "HumanLavinian-Bowman",
    (Archer, Professional) -> "HumanLavinian-SiegeArcher",
    (FireWizard, Militia) -> "HumanCeresian-Acolyte",
    (FireWizard, Professional) -> "HumanCeresian-Deacon",
    (BladeCavalry, Militia) -> "HumanWesnoth-Cavalryman",
    (BladeCavalry, Professional) -> "HumanWesnoth-Dragoon"
  ))

  val WesnothCulture = WarriorViewNames(Map(
    (HeavyBladeInfantry, Militia) -> "HumanWesnoth-Sergeant",
    (HeavyBladeInfantry, Professional) -> "HumanWesnoth-General",
    (LightBladeInfantry, Militia) -> "HumanWesnoth-Fencer",
    (LightBladeInfantry, Professional) -> "HumanWesnoth-MasterAtArms",
    (Archer, Militia) -> "HumanWesnoth-Bowman",
    (Archer, Professional) -> "HumanWesnoth-Longbowman",
    (FireWizard, Militia) -> "HumanWesnoth-Mage",
    (FireWizard, Professional) -> "HumanWesnoth-RedMage",
    (PikeCavalry, Militia) -> "HumanWesnoth-Horseman",
    (PikeCavalry, Professional) -> "HumanWesnoth-Knight"
  ))

  val DarkHumanCulture = WarriorViewNames(Map(
    (HeavyMaceInfantry, Militia) -> "HumanWesnoth-HeavyInfantryman",
    (HeavyMaceInfantry, Professional) -> "HumanWesnoth-Shocktrooper",
    (LightBladeInfantry, Militia) -> "HumanChaos-Invader",
    (LightBladeInfantry, Professional) -> "HumanChaos-Knight",
    (Archer, Militia) -> "HumanDark-Bowman",
    (Archer, Professional) -> "HumanDark-HeavyBowman",
    (FireWizard, Militia) -> "HumanChaos-Invoker",
    (FireWizard, Professional) -> "HumanChaos-Magus",
    (PikeCavalry, Militia) -> "HumanDark-Knight",
    (PikeCavalry, Professional) -> "HumanDark-Cavalier"
    //(HeavyBladeInfantry, Militia) -> "HumanDark-Soldier",
    //(HeavyBladeInfantry, Professional) -> "HumanDark-Champion",
  ))
}