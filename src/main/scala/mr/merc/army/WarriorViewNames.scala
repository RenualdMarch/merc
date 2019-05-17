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
    (PikeCavalry, Professional) -> "HumanDark-Cavalier",
    (HeavyBladeInfantry, Militia) -> "HumanDark-Soldier",
    (HeavyBladeInfantry, Professional) -> "HumanDark-Champion"
  ))

  val GreekDardoCulture = WarriorViewNames(Map(
    (HeavyBladeInfantry, Militia) -> "HumanDardo-Praetorian",
    (HeavyBladeInfantry, Professional) -> "HumanDardo-Justicar",
    (LightBladeInfantry, Militia) -> "HumanDardo-Sellsword",
    (LightBladeInfantry, Professional) -> "HumanDardo-Mercenary",
    (Archer, Militia) -> "HumanDardo-Bowman",
    (Archer, Professional) -> "HumanDardo-Longbowman",
    (HeavyPikeInfantry, Militia) -> "HumanDardo-Hoplite",
    (HeavyPikeInfantry, Professional) -> "HumanDardo-Phalanx",
    (BladeCavalry, Militia) -> "HumanDardo-Rider",
    (BladeCavalry, Professional) -> "HumanDardo-HeavyCavalier",
  ))

  val ChevalierCulture = WarriorViewNames(Map(
    (HeavyPikeInfantry, Militia) -> "HumanChevalier-Miliz",
    (HeavyPikeInfantry, Professional) -> "HumanChevalier-Halberdier",
    (HeavyBladeInfantry, Militia) -> "HumanChevalier-Soldier",
    (HeavyBladeInfantry, Professional) -> "HumanChevalier-Swordsman",
    (Archer, Militia) -> "HumanChevalier-Bowman",
    (Archer, Professional) -> "HumanChevalier-Siegebowman",
    (FireWizard, Militia) -> "HumanChevalier-Quack",
    (FireWizard, Professional) -> "HumanChevalier-Doctor",
    (PikeCavalry, Militia) -> "HumanChevalier-Horseman",
    (PikeCavalry, Professional) -> "HumanChevalier-Lancer",
    (BladeCavalry, Militia) -> "HumanChevalier-Rider",
    (BladeCavalry, Professional) -> "HumanChevalier-Chevalier",
  ))

  val ArabCulture = WarriorViewNames(Map(
    (HeavyPikeInfantry, Militia) -> "HumanDuneWesnoth-Spearguard",
    (HeavyPikeInfantry, Professional) -> "HumanDuneWesnoth-Spearmaster",
    (PikeCavalry, Militia) -> "HumanDuneWesnoth-Piercer",
    (PikeCavalry, Professional) -> "HumanDuneWesnoth-Cataphract",
    (FireWizard, Militia) -> "HumanDuneWesnoth-Apothecary",
    (FireWizard, Professional) -> "HumanDuneWesnoth-Herbalist",
    (HeavyBladeInfantry, Militia) -> "HumanDuneWesnoth-Swordsman",
    (HeavyBladeInfantry, Professional) -> "HumanDuneWesnoth-Soldier",
    (LightBladeInfantry, Militia) -> "HumanDuneWesnoth-Skirmisher",
    (LightBladeInfantry, Professional) -> "HumanDuneWesnoth-Warmaster"
  ))

  val SlavicCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "HumanCeresians-Cavalryman",
    (BladeCavalry, Professional) -> "HumanCeresians-Knight-Commander",
    (FireWizard, Militia) -> "HumanMountain-Mage",
    (FireWizard, Professional) -> "HumanMountain-Eremit",
    (Archer, Militia) -> "HumanMountain-Hunter",
    (Archer, Professional) -> "HumanMountain-Ranger",
    (LightBladeInfantry, Militia) -> "HumanMountain-Wolf-Warrior",
    (LightBladeInfantry, Professional) -> "HumanMountain-Wolf-Champion",
    (HeavyPikeInfantry, Militia) -> "HumanMountain-Wolf-Fighter",
    (HeavyPikeInfantry, Professional) -> "HumanMountain-Wolf-Swordmaster",
  ))
}