package mr.merc.army

import mr.merc.army.WarriorCompetence._
import mr.merc.army.WarriorType._

case class WarriorViewNames(possible:Map[(WarriorType, WarriorCompetence), String]) {

  lazy val possibleWarriors:Map[(WarriorType, WarriorCompetence), String] =
    possible.filter(p => Set[WarriorCompetence](WarriorCompetence.Militia, WarriorCompetence.Professional) contains(p._1._2))

  lazy val possibleRulers:Map[(WarriorType, WarriorCompetence), String] =
    possible.filter(_._1._2 == WarriorCompetence.Ruler)

  lazy val possibleNobles:Map[(WarriorType, WarriorCompetence), String] =
    possible.filter(_._1._2 == WarriorCompetence.Professional)
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
    (BladeCavalry, Professional) -> "HumanWesnoth-Dragoon",

    (HeavyPikeInfantry, Ruler) -> "HumanLavinian-Centurion",
    (LightBladeInfantry, Ruler) -> "HumanLavinian-Aquilifer",
    (HeavyBladeInfantry, Ruler) -> "HumanLavinian-Praetorian"
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
    (PikeCavalry, Professional) -> "HumanWesnoth-Knight",

    (PikeCavalry, Ruler) -> "HumanWesnoth-GrandKnight",
    (HeavyBladeInfantry, Ruler) -> "HumanWesnoth-GrandMarshal",
    (FireWizard, Ruler) -> "HumanWesnoth-GreatMage"

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
    (HeavyBladeInfantry, Professional) -> "HumanDark-Champion", //"HumanDark-Longswordsman",
    (HeavyPikeInfantry, Militia) -> "HumanDark-Spearman",
    (HeavyPikeInfantry, Professional) -> "HumanDark-Halberdier",

    (HeavyBladeInfantry, Ruler) -> "HumanChaos-HellGuardian",
    (HeavyPikeInfantry, Ruler) -> "HumanDark-Deathmaster",
    (HeavyMaceInfantry, Ruler) -> "HumanWesnoth-IronMauler"
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

    (BladeCavalry, Ruler) -> "HumanDardo-Cavalier",
    (HeavyPikeInfantry, Ruler) -> "HumanDardo-Captain",
    (HeavyMaceInfantry, Ruler) -> "HumanWesnoth-RoyalWarrior" // - ?
  ))

  val ChevalierCulture = WarriorViewNames(Map(
    (HeavyPikeInfantry, Militia) -> "HumanChevalier-Miliz",
    (HeavyPikeInfantry, Professional) -> "HumanChevalier-Pikeman",
    (HeavyBladeInfantry, Militia) -> "HumanChevalier-Soldier",
    (HeavyBladeInfantry, Professional) -> "HumanChevalier-Swordsman",
    (BladeArcher, Militia) -> "HumanChevalier-Bowman",
    (BladeArcher, Professional) -> "HumanChevalier-Siegebowman",
    (FireWizard, Militia) -> "HumanChevalier-Quack",
    (FireWizard, Professional) -> "HumanChevalier-Doctor",
    (PikeCavalry, Militia) -> "HumanChevalier-Horseman",
    (PikeCavalry, Professional) -> "HumanChevalier-Lancer",
    (BladeCavalry, Militia) -> "HumanChevalier-Rider",
    (BladeCavalry, Professional) -> "HumanChevalier-HeavyRider",

    (BladeCavalry, Ruler) -> "HumanChevalier-Chevalier",
    (PikeCavalry, Ruler) -> "HumanChevalier-GrandKnight",
    (HeavyBladeInfantry, Ruler) -> "HumanChevalier-Greatsword",
    (HeavyPikeInfantry, Ruler) -> "HumanChevalier-Halberdier"

  ))

  val ArabCulture = WarriorViewNames(Map(
    (HeavyPikeInfantry, Militia) -> "HumanDuneWesnoth-Spearguard",
    (HeavyPikeInfantry, Professional) -> "HumanDuneWesnoth-Spearmaster",
    (PikeCavalry, Militia) -> "HumanDuneWesnoth-Piercer",
    (PikeCavalry, Professional) -> "HumanDuneWesnoth-Sunderer", //"HumanDuneWesnoth-Cataphract",
    (FireWizard, Militia) -> "HumanDuneWesnoth-Apothecary",
    (FireWizard, Professional) -> "HumanDuneWesnoth-Herbalist",
    (HeavyBladeInfantry, Militia) -> "HumanDuneWesnoth-Swordsman",
    (HeavyBladeInfantry, Professional) -> "HumanDuneWesnoth-Soldier",
    (LightBladeInfantry, Militia) -> "HumanDuneWesnoth-Skirmisher",
    (LightBladeInfantry, Professional) -> "HumanDuneWesnoth-Warmaster",

    (PikeCavalry, Ruler) -> "HumanDuneWesnoth-Cataphract",
    (BladeCavalry, Ruler) -> "HumanDuneWesnoth-Marauder",
    (HeavyBladeInfantry, Ruler) -> "HumanDuneWesnoth-Ranger"
  ))

  val WolfCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "HumanCeresians-Cavalryman",
    (BladeCavalry, Professional) -> "HumanCeresians-Knight-Errant",
    (FireWizard, Militia) -> "HumanMountain-Mage",
    (FireWizard, Professional) -> "HumanMountain-Eremit",
    (BladeArcher, Militia) -> "HumanMountain-Hunter",
    (BladeArcher, Professional) -> "HumanMountain-Ranger",
    (LightBladeInfantry, Militia) -> "HumanMountain-Wolf-Warrior",
    (LightBladeInfantry, Professional) -> "HumanMountain-Wolf-Champion",
    (HeavyPikeInfantry, Militia) -> "HumanMountain-Wolf-Fighter",
    (HeavyPikeInfantry, Professional) -> "HumanMountain-Wolf-Swordmaster",

    (BladeCavalry, Ruler) -> "HumanCeresians-Knight-Commander",
    (FireWizard, Ruler) -> "HumanCeresians-Metropolitan",
    (HeavyBladeInfantry, Ruler) -> "HumanCeresians-Champion"
  ))

  val LuzCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "HumanLuz-Rider",
    (BladeCavalry, Professional) -> "HumanLuz-Knight",
    (FireWizard, Militia) -> "HumanLuz-Cleric",
    (FireWizard, Professional) -> "HumanLuz-Bishop",
    (MaceArcher, Militia) -> "HumanLuz-Crossbow",
    (MaceArcher, Professional) -> "HumanLuz-Arbalist",
    (LightBladeInfantry, Militia) -> "HumanLuz-Inquisition-Warrior",
    (LightBladeInfantry, Professional) -> "HumanLuz-Inquisitor",
    (HeavyPikeInfantry, Militia) -> "HumanLuz-Miliz",
    (HeavyPikeInfantry, Professional) -> "HumanLuz-Keeper",

    (FireWizard, Ruler) -> "HumanLuz-Pope",
    (HeavyPikeInfantry, Ruler) -> "HumanLuz-Sentinel",
    (BladeCavalry, Ruler) -> "HumanLuz-Paladin",
    (HeavyMaceInfantry, Ruler) -> "HumanLuz-HolyWarrior"
  ))

  val WoodElvesCulture = WarriorViewNames(Map(
    (ElvenArcher, Militia) -> "ElvesWesnoth-Archer",
    (ElvenArcher, Professional) -> "ElvesWesnoth-Marksman",
    (ElvenFighter, Militia) -> "ElvesWesnoth-Fighter",
    (ElvenFighter, Professional) -> "ElvesWesnoth-Hero",
    (FireWizard, Militia) -> "ElvesWesnoth-Shaman",
    (FireWizard, Professional) -> "ElvesWesnoth-Sylph",
    (BladeCavalry, Militia) -> "ElvesWesnoth-Scout",
    (BladeCavalry, Professional) -> "ElvesWesnoth-Rider",

    (ElvenArcher, Ruler) -> "ElvesWesnoth-Sharpshooter",
    (BladeCavalry, Ruler) -> "ElvesWesnoth-Outrider",
    (ElvenFighter, Ruler) -> "ElvesWesnoth-Marshal",
  ))

  val DesertElvesCulture = WarriorViewNames(Map(
    (ElvenArcher, Militia) -> "ElvesDesert-Archer",
    (ElvenArcher, Professional) -> "ElvesDesert-Marksman",
    (ElvenFighter, Militia) -> "ElvesDesert-Fighter",
    (ElvenFighter, Professional) -> "ElvesDesert-Hero",
    (FireWizard, Militia) -> "ElvesDesert-Shaman",
    (FireWizard, Professional) -> "ElvesDesert-Druid",
    (BladeCavalry, Militia) -> "ElvesDesert-Scout",
    (BladeCavalry, Professional) -> "ElvesDesert-Rider",

    (ElvenArcher, Ruler) -> "ElvesDesert-Sharpshooter",
    (BladeCavalry, Ruler) -> "ElvesDesert-Outrider",
    (ElvenFighter, Ruler) -> "ElvesDesert-Marshal"
  ))

  val WesnothOrcCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "OrcsAgeless-Oxrider",
    (BladeCavalry, Professional) -> "OrcsAgeless-Destrier",
    (FireWizard, Militia) -> "OrcsAgeless-Drifter",
    (FireWizard, Professional) -> "OrcsAgeless-Wanderer",
    (HeavyBladeInfantry, Militia) -> "OrcsWesnoth-Grunt",
    (HeavyBladeInfantry, Professional) -> "OrcsWesnoth-Warrior",
    (LightBladeInfantry, Militia) -> "OrcsWesnoth-Assassin",
    (LightBladeInfantry, Professional) -> "OrcsWesnoth-Slayer",
    (BladeArcher, Militia) -> "OrcsWesnoth-Archer",
    (BladeArcher, Professional) -> "OrcsWesnoth-Crossbow",

    (BladeArcher, Ruler) -> "OrcsWesnoth-Slurbow",
    (HeavyBladeInfantry, Ruler) -> "OrcsWesnoth-Warlord"
  ))

  val LatinOrcCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "OrcsLatin-Essedarius",
    (BladeCavalry, Professional) -> "OrcsLatin-EssedariusVulpis",
    (FireWizard, Militia) -> "OrcsLatin-Shaman",
    (FireWizard, Professional) -> "OrcsLatin-Elder",
    (HeavyBladeInfantry, Militia) -> "OrcsLatin-Secutor",
    (HeavyBladeInfantry, Professional) -> "OrcsLatin-SecutorLeonis",
    (LightBladeInfantry, Militia) -> "OrcsLatin-Gallus",
    (LightBladeInfantry, Professional) -> "OrcsLatin-Dimacherius",
    (BladeArcher, Militia) -> "OrcsLatin-Venator",
    (BladeArcher, Professional) -> "OrcsLatin-VenatorFaber",

    (HeavyBladeInfantry, Ruler) -> "OrcsWesnoth-Sovereign",
  ))

  val WesnothDwarfCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "DwarfWesnoth-Rider",
    (BladeCavalry, Professional) -> "DwarfWesnoth-Master",
    (FireWizard, Militia) -> "DwarfWesnoth-Runesmith",
    (FireWizard, Professional) -> "DwarfWesnoth-Runemaster",
    (HeavyBladeInfantry, Militia) -> "DwarfWesnoth-Fighter",
    (HeavyBladeInfantry, Professional) -> "DwarfWesnoth-Steelclad",
    (LightBladeInfantry, Militia) -> "DwarfWesnoth-Ulfserker",
    (LightBladeInfantry, Professional) -> "DwarfWesnoth-Berserker",
    (HeavyPikeInfantry, Militia) -> "DwarfWesnoth-Guardsman",
    (HeavyPikeInfantry, Professional) -> "DwarfWesnoth-Sentinel",

    (FireWizard, Ruler) -> "DwarfWesnoth-Arcanister",
    (HeavyBladeInfantry, Ruler) -> "DwarfWesnoth-Lord"
  ))

  val WesnothUndeadCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "UndeadWesnoth-Rider",
    (BladeCavalry, Professional) -> "UndeadWesnoth-Deathrider",
    (FireWizard, Militia) -> "UndeadWesnoth-Lich",
    (FireWizard, Professional) -> "UndeadWesnoth-AncientLich",
    (HeavyBladeInfantry, Militia) -> "UndeadWesnoth-Revenant",
    (HeavyBladeInfantry, Professional) -> "UndeadWesnoth-Draug",
    (LightBladeInfantry, Militia) -> "UndeadWesnoth-Skeleton",
    (LightBladeInfantry, Professional) -> "UndeadWesnoth-Deathblade",
    (BladeArcher, Militia) -> "UndeadWesnoth-Boneshooter",
    (BladeArcher, Professional) -> "UndeadWesnoth-Banebow",

    (FireWizard, Ruler) -> "UndeadWesnoth-Necromancer"
  ))

  val DarkElvesCulture = WarriorViewNames(Map(
    (BladeCavalry, Militia) -> "DarkElvesEE-LizardRider",
    (BladeCavalry, Professional) -> "DarkElvesEE-LizardMaster",
    (FireWizard, Militia) -> "DarkElvesEE-Enchantress",
    (FireWizard, Professional) -> "DarkElvesEE-Cleric",
    (HeavyBladeInfantry, Militia) -> "DarkElvesEE-Lord",
    (HeavyBladeInfantry, Professional) -> "DarkElvesEE-HighLord",
    (LightBladeInfantry, Militia) -> "DarkElvesEE-Fighter",
    (LightBladeInfantry, Professional) -> "DarkElvesEE-Corsair",
    (BladeArcher, Militia) -> "DarkElvesEE-Hunter",
    (BladeArcher, Professional) -> "DarkElvesEE-Shadow",

    (LightBladeInfantry, Ruler) -> "DarkElvesEE-Marshal"
  ))
}