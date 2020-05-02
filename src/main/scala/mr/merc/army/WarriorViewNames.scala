package mr.merc.army

import mr.merc.army.WarriorCompetence._
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
    (BladeArcher, Militia) -> "HumanLavinian-Bowman",
    (BladeArcher, Professional) -> "HumanLavinian-SiegeArcher",
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
    (BladeArcher, Militia) -> "HumanWesnoth-Bowman",
    (BladeArcher, Professional) -> "HumanWesnoth-Longbowman",
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
    (BladeArcher, Militia) -> "HumanDark-Bowman",
    (BladeArcher, Professional) -> "HumanDark-HeavyBowman",
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
    (BladeArcher, Militia) -> "HumanDardo-Bowman",
    (BladeArcher, Professional) -> "HumanDardo-Longbowman",
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
    (BladeArcher, Militia) -> "HumanChevalier-Bowman",
    (BladeArcher, Professional) -> "HumanChevalier-Siegebowman",
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

  val WolfCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "HumanCeresians-Cavalryman",
    (BladeCavalry, Professional) -> "HumanCeresians-Knight-Commander",
    (FireWizard, Militia) -> "HumanMountain-Mage",
    (FireWizard, Professional) -> "HumanMountain-Eremit",
    (BladeArcher, Militia) -> "HumanMountain-Hunter",
    (BladeArcher, Professional) -> "HumanMountain-Ranger",
    (LightBladeInfantry, Militia) -> "HumanMountain-Wolf-Warrior",
    (LightBladeInfantry, Professional) -> "HumanMountain-Wolf-Champion",
    (HeavyPikeInfantry, Militia) -> "HumanMountain-Wolf-Fighter",
    (HeavyPikeInfantry, Professional) -> "HumanMountain-Wolf-Swordmaster",
  ))

  val LuzCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "HumanLuz-Rider",
    (BladeCavalry, Professional) -> "HumanLuz-Knight",
    (FireWizard, Militia) -> "HumanLuz-Cleric",
    (FireWizard, Professional) -> "HumanLuz-Bishop",
    (MaceArcher, Militia) -> "HumanLuz-crossbow",
    (MaceArcher, Professional) -> "HumanLuz-Arbalist",
    (LightBladeInfantry, Militia) -> "HumanLuz-Inquisition-Warrior",
    (LightBladeInfantry, Professional) -> "HumanLuz-Inquisitor",
    (HeavyPikeInfantry, Militia) -> "HumanLuz-Keeper",
    (HeavyPikeInfantry, Professional) -> "HumanLuz-Sentinel",
  ))

  val WoodElvesCulture = WarriorViewNames(Map(
    (ElvenArcher, Militia) -> "ElvesWesnoth-Archer",
    (ElvenArcher, Professional) -> "ElvesWesnoth-Sharpshooter",
    (ElvenFighter, Militia) -> "ElvesWesnoth-Fighter",
    (ElvenFighter, Professional) -> "ElvesWesnoth-Hero",
    (FireWizard, Militia) -> "ElvesWesnoth-Shaman",
    (FireWizard, Professional) -> "ElvesWesnoth-Sylph",
    (BladeCavalry, Militia) -> "ElvesWesnoth-Scout",
    (BladeCavalry, Professional) -> "ElvesWesnoth-Outrider"
  ))

  val DesertElvesCulture = WarriorViewNames(Map(
    (ElvenArcher, Militia) -> "ElvesDesert-Archer",
    (ElvenArcher, Professional) -> "ElvesDesert-Sharpshooter",
    (ElvenFighter, Militia) -> "ElvesDesert-Fighter",
    (ElvenFighter, Professional) -> "ElvesDesert-Hero",
    (FireWizard, Militia) -> "ElvesDesert-Shaman",
    (FireWizard, Professional) -> "ElvesDesert-Druid",
    (BladeCavalry, Militia) -> "ElvesDesert-Scout",
    (BladeCavalry, Professional) -> "ElvesDesert-Outrider"
  ))
}