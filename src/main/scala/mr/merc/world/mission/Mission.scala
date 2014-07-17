package mr.merc.world.mission

import java.time.LocalDate
import mr.merc.world.Country
import scala.util.Random
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe._
import scala.reflect._
import mr.merc.map.world.Province
import mr.merc.util.MercUtils._
import java.time.Period
import mr.merc.world.WorldState

sealed trait Mission {
  def difficulty: Difficulty
  def location: Province
  def comparingObj: AnyRef
}

case class DefendFromEnemyMission(difficulty: Difficulty, location: Province, enemyProvince: Province) extends Mission {
  def comparingObj = (location, enemyProvince)
}
case class AttackEnemyMission(difficulty: Difficulty, location: Province, enemyProvince: Province) extends Mission {
  def comparingObj = (location, enemyProvince)
}
case class SupressRiotMission(difficulty: Difficulty, location: Province) extends Mission {
  def comparingObj = location
}
case class KillAnimalsMission(difficulty: Difficulty, location: Province) extends Mission {
  def comparingObj = location
}
case class GuardCaravanMission(difficulty: Difficulty, location: Province) extends Mission {
  def comparingObj = location
}

case class MissionContext(worldState: WorldState, location: Province)

object MissionGenerators {

  val missionGenerators = Map(classOf[AttackEnemyMission] -> attackEnemyMission _,
    classOf[DefendFromEnemyMission] -> defendFromEnemyMission _,
    classOf[SupressRiotMission] -> supressRiotMission _,
    classOf[KillAnimalsMission] -> killAnimals _)

  private def daysForExecution = 30 + Random.nextInt(15)

  // TODO move to config file
  private val bounds = Map(classOf[AttackEnemyMission] -> DifficultyBound(60, 90),
    classOf[SupressRiotMission] -> DifficultyBound(40, 60),
    classOf[KillAnimalsMission] -> DifficultyBound(20, 50),
    classOf[GuardCaravanMission] -> DifficultyBound(30, 60),
    classOf[DefendFromEnemyMission] -> DifficultyBound(70, 100))

  def endDateByStartDate(startDate: LocalDate) = startDate.plusDays(daysForExecution)

  def attackEnemyMission(ctx: MissionContext): Option[AttackEnemyMission] = {
    val diff = bounds(classOf[AttackEnemyMission])
    val currentCountry = ctx.worldState.map.countryByProvince(ctx.location)
    val neigs = ctx.worldState.map.neighbours(ctx.location).map(ctx.worldState.map.countryByProvince)
    val enemies = neigs.filter(n => ctx.worldState.diplomacyEngine.areEnemies(currentCountry, n))
    if (enemies.isEmpty) {
      None
    } else {
      val enemy = enemies.randomElement()
      val enemyProvince = ctx.worldState.map.neighboursByCountry(ctx.location, enemy).randomElement()
      Some(AttackEnemyMission(diff.generateDifficulty, ctx.location, enemyProvince))
    }
  }

  def defendFromEnemyMission(ctx: MissionContext): Option[DefendFromEnemyMission] = {
    val diff = bounds(classOf[DefendFromEnemyMission])
    val currentCountry = ctx.worldState.map.countryByProvince(ctx.location)
    val neigs = ctx.worldState.map.neighbours(ctx.location).map(ctx.worldState.map.countryByProvince)
    val enemies = neigs.filter(n => ctx.worldState.diplomacyEngine.areEnemies(currentCountry, n))
    if (enemies.isEmpty) {
      None
    } else {
      val enemy = enemies.randomElement()
      val enemyProvince = ctx.worldState.map.neighboursByCountry(ctx.location, enemy).randomElement()
      Some(DefendFromEnemyMission(diff.generateDifficulty, ctx.location, enemyProvince))
    }
  }

  def supressRiotMission(ctx: MissionContext): Option[SupressRiotMission] = {
    val diff = bounds(classOf[SupressRiotMission])
    Some(SupressRiotMission(diff.generateDifficulty, ctx.location))
  }

  def killAnimals(ctx: MissionContext): Option[KillAnimalsMission] = {
    val diff = bounds(classOf[KillAnimalsMission])
    Some(KillAnimalsMission(diff.generateDifficulty, ctx.location))
  }
}

case class Difficulty(diff: Int) {
  def reward = diff * 100
}

case class DifficultyBound(from: Int, to: Int) {
  def generateDifficulty: Difficulty = Difficulty(from + Random.nextInt(to - from))
}