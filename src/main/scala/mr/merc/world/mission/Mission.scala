package mr.merc.world.mission

import java.time.LocalDate
import scalaz._
import mr.merc.world.Country

trait Mission {
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