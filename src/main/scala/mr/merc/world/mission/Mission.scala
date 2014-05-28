package mr.merc.world.mission

import java.time.LocalDate
import scalaz._
import mr.merc.world.Country
import scala.util.Random
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe._
import scala.reflect._

sealed trait Mission {
  def startDate: LocalDate
  def endDate: LocalDate
  def daysForExecution: Int = 30
  def difficulty: Int @@ Difficulty
  def enemyFaction: Country
}

class AttackEnemyMission(val startDate: LocalDate, val endDate: LocalDate, val difficulty: Int @@ Difficulty, val enemyFaction: Country) extends Mission
class SupressRiotMission(val startDate: LocalDate, val endDate: LocalDate, val difficulty: Int @@ Difficulty, val enemyFaction: Country) extends Mission
class KillAnimalsMission(val startDate: LocalDate, val endDate: LocalDate, val difficulty: Int @@ Difficulty, val enemyFaction: Country) extends Mission
class GuardCaravanMission(val startDate: LocalDate, val endDate: LocalDate, val difficulty: Int @@ Difficulty, val enemyFaction: Country) extends Mission

sealed trait Difficulty {
  self: Int @@ Difficulty =>
  // TODO think about formula
  def reward = self * 100
}

object DifficultyBound {
  val bounds = Map(classOf[AttackEnemyMission] -> DifficultyBound(80, 100),
    classOf[SupressRiotMission] -> DifficultyBound(60, 80),
    classOf[KillAnimalsMission] -> DifficultyBound(20, 50),
    classOf[GuardCaravanMission] -> DifficultyBound(30, 60))
}

case class DifficultyBound(from: Int, to: Int) {
  def generateDifficulty: Int @@ Difficulty = Tag[Int, Difficulty](from + Random.nextInt(to - from))
}