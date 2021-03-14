package mr.merc.army

import mr.merc.army.WarriorCompetence._
import mr.merc.economics.Culture
import mr.merc.map.terrain._
import mr.merc.unit._

abstract class WarriorType(val name: String) {

  private def viewName(culture: Culture, competence: WarriorCompetence): String = {
    culture.warriorViewNames.possibleWarriors(this, competence)
  }

  def baseHp: Int

  import mr.merc.economics.WorldConstants.Technology.increaseValue

  def hp(competence: WarriorCompetence, techLevel: Int): Int = {
    increaseValue(competence match {
      case Professional => baseHp
      case Militia => baseHp * 2 / 3
    }, techLevel)
  }

  def movement: Int

  def baseAttacks: List[Attack]


  private def attacks(competence: WarriorCompetence, techLevel: Int): List[Attack] = {
    (competence match {
      case Professional => baseAttacks
      case Militia => baseAttacks.map { at =>
        at.copy(damage = at.damage * 2 / 3)
      }
    }).map { attack =>
      attack.copy(damage = increaseValue(attack.damage, techLevel))
    }
  }
  def moveCost: Map[TerrainKind, Int]
  def defence: Map[DefenceType, Int]
  def resistance: Map[AttackType, Int]
  def attributes: Set[SoldierTypeAttribute]

  private def level(competence: WarriorCompetence) = competence match {
    case Militia => 1
    case Professional => 2
  }

  def buildSoldierType(c:WarriorCompetence, culture: Culture, techLevel: Int):SoldierType = SoldierType(
    s"$name $techLevel", level(c), hp(c, techLevel), movement, 0, level(c),
    attacks(c, techLevel), moveCost, defence, resistance, attributes, viewName(culture, c))
}


sealed trait WarriorCompetence

object WarriorCompetence {
  case object Militia extends WarriorCompetence
  case object Professional extends WarriorCompetence
}



object WarriorType {

  def allWarriorTypes:List[WarriorType] = List(HeavyBladeInfantry, LightBladeInfantry, BladeArcher, MaceArcher,
    BladeCavalry, PikeCavalry, FireWizard, HeavyMaceInfantry, HeavyPikeInfantry, ElvenFighter, ElvenArcher)

  case object HeavyBladeInfantry extends WarriorType("heavyBladeInfantry") {

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 2, SwampKind -> 3, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 3, IceKind -> movement)

    override def baseAttacks = List(Attack(0, 9, 4, Blade, ranged = false))

    override def baseHp: Int = 55

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 50, SwampDefence -> 20, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 60,
      SnowDefence -> 20, IceDefence -> 10)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 20, Pierce -> 0, Impact -> 20, Fire -> 0, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()

    override def movement: Int = 5
  }

  case object HeavyMaceInfantry extends WarriorType("heavyMaceInfantry") {
    override def baseHp: Int = 52

    override def movement: Int = 4

    override def baseAttacks: List[Attack] =  List(Attack(0, 15, 2, Impact, ranged = false))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 4, ForestKind -> 2, SwampKind -> 4, HillKind -> 3,
      MountainKind -> 4, SandKind -> 2, GrassKind -> 1, WallsKind -> 1, SnowKind -> 3, IceKind -> movement)

    override def defence: Map[DefenceType, Int] =  Map(
      WaterDefence -> 20, ForestDefence -> 40, SwampDefence -> 20, HillDefence -> 40,
      MountainDefence -> 50, SandDefence -> 20, GrassDefence -> 30, BuildingDefence -> 40,
      SnowDefence -> 20, IceDefence -> 10)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 50, Pierce -> 40, Impact -> 10, Fire -> -10, Cold -> -10, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object HeavyPikeInfantry extends WarriorType("heavyPikeInfantry") {
    override def baseHp: Int = 57

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = List(Attack(0, 10, 3, Pierce, ranged = false, Set(Firststrike)))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 2, SwampKind -> 3, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 3, IceKind -> movement)

    override def defence: Map[DefenceType, Int] =  Map(
      WaterDefence -> 20, ForestDefence -> 50, SwampDefence -> 20, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 60,
      SnowDefence -> 20, IceDefence -> 10)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 40, Impact -> 0, Fire -> 0, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object LightBladeInfantry extends WarriorType("lightBladeInfantry") {
    override def baseHp: Int = 47

    override def movement: Int = 7

    override def baseAttacks: List[Attack] = List(Attack(0, 8, 5, Blade, ranged = false))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 2, ForestKind -> 2, SwampKind -> 2, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 2, IceKind -> 3)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 40, ForestDefence -> 70, SwampDefence -> 40, HillDefence -> 70,
      MountainDefence -> 70, SandDefence -> 40, GrassDefence -> 60, BuildingDefence -> 70,
      SnowDefence -> 40, IceDefence -> 30)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> -30, Pierce -> -20, Impact -> -20, Fire -> 0, Cold -> 10, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set(Skirmisher)
  }

  case object BladeArcher extends WarriorType("bladeArcher") {
    override def baseHp: Int = 51

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = List(
      Attack(0, 8, 2, Blade, ranged = false),
      Attack(1, 8, 3, Pierce, ranged = true))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 2, SwampKind -> 3, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 3, IceKind -> 4)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 50, SwampDefence -> 20, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 70,
      SnowDefence -> 20, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 0, Impact -> 0, Fire -> 0, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object MaceArcher extends WarriorType("maceArcher") {
    override def baseHp: Int = 51

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = List(
      Attack(0, 8, 2, Impact, ranged = false),
      Attack(1, 8, 3, Impact, ranged = true))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 2, SwampKind -> 3, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 3, IceKind -> 4)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 50, SwampDefence -> 20, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 70,
      SnowDefence -> 20, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 0, Impact -> 0, Fire -> 0, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object BladeCavalry extends WarriorType("bladeCavalry") {
    override def baseHp: Int = 65

    override def movement: Int = 9

    override def baseAttacks: List[Attack] = List(Attack(0, 10, 4, Blade, ranged = false))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 4, ForestKind -> 3, SwampKind -> 4, HillKind -> 2,
      MountainKind -> 9, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 2, IceKind -> 3)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 30, SwampDefence -> 20, HillDefence -> 40,
      MountainDefence -> 40, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 40,
      SnowDefence -> 30, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 30, Pierce -> -20, Impact -> 40, Fire -> 0, Cold -> 20, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object PikeCavalry extends WarriorType("pikeCavalry") {
    override def baseHp: Int = 58

    override def movement: Int = 8

    override def baseAttacks: List[Attack] = List(Attack(0, 14, 2, Pierce, ranged = false, Set(Charge)))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 4, ForestKind -> 3, SwampKind -> 4, HillKind -> 2,
      MountainKind -> 8, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 2, IceKind -> 3)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 30, SwampDefence -> 20, HillDefence -> 40,
      MountainDefence -> 40, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 40,
      SnowDefence -> 30, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 20, Pierce -> -20, Impact -> 30, Fire -> 0, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object FireWizard extends WarriorType("fireWizard") {
    override def baseHp: Int = 39

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = List(
      Attack(0, 6, 2, Impact, ranged = false, Set()),
      Attack(1, 7, 4, Fire, ranged = true, Set(Magical)))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 2, SwampKind -> 3, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 3, IceKind -> 4)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 50, SwampDefence -> 20, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 60,
      SnowDefence -> 20, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 0, Impact -> 0, Fire -> 10, Cold -> 0, Arcane -> 20)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object ElvenFighter extends WarriorType("elvenFighter") {
    override def baseHp: Int = 53

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = List(
      Attack(0, 7, 4, Blade, ranged = false, Set()),
      Attack(1, 6, 3, Pierce, ranged = true, Set()))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 1, SwampKind -> 2, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 2, IceKind -> 3)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 60, SwampDefence -> 30, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 60,
      SnowDefence -> 30, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 0, Impact -> 0, Fire -> 0, Cold -> 0, Arcane -> -10)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  case object ElvenArcher extends WarriorType("elvenArcher") {
    override def baseHp: Int = 40

    override def movement: Int = 6

    override def baseAttacks: List[Attack] = List(
      Attack(0, 6, 2, Blade, ranged = false, Set()),
      Attack(1, 9, 4, Pierce, ranged = true, Set()))

    override def moveCost: Map[TerrainKind, Int] = Map(
      WaterKind -> 3, ForestKind -> 1, SwampKind -> 2, HillKind -> 2,
      MountainKind -> 3, SandKind -> 2, GrassKind -> 1, WallsKind -> 1,
      SnowKind -> 2, IceKind -> 3)

    override def defence: Map[DefenceType, Int] = Map(
      WaterDefence -> 20, ForestDefence -> 70, SwampDefence -> 30, HillDefence -> 50,
      MountainDefence -> 60, SandDefence -> 30, GrassDefence -> 40, BuildingDefence -> 60,
      SnowDefence -> 30, IceDefence -> 20)

    override def resistance: Map[AttackType, Int] = Map(
      Blade -> 0, Pierce -> 0, Impact -> 0, Fire -> 0, Cold -> 0, Arcane -> -10)

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }
}
