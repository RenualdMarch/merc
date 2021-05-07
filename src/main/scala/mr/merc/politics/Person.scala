package mr.merc.politics

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.economics.Population.Race
import mr.merc.economics.{Culture, SeasonOfYear, Seasons}
import scalafx.scene.image.{Image, ImageView}

import scala.util.Random

class Person(val name: String, val state: State, val culture: Culture, wt: WarriorType, wc: WarriorCompetence,
             val born: SeasonOfYear, val projectedDeath: SeasonOfYear) {

  private var killed: Option[Int] = None

  def kill(turn: Int): Unit = {
    killed = Some(turn)
  }

  def isAlive(turn: Int): Boolean = {
    killed.isEmpty && turn < projectedDeath.turn
  }

  def age(turn: Int): Int = {
    val end = if (isAlive(turn)) turn
    else killed.getOrElse(projectedDeath.turn)
    (end - born.turn) / 4
  }

  def fullName:String = name

  @transient
  lazy val largeImage:Image = new Warrior(wt, wc, culture, state).image(2d)

  @transient
  lazy val mediumImage:Image = new Warrior(wt, wc, culture, state).image(1.5)

  @transient
  lazy val smallImage:Image = new Warrior(wt, wc, culture, state).image(1d)
}

object Person {

  def generateBirthAndDeath(turn:Int, race: Race): (SeasonOfYear, SeasonOfYear) = {
    val currentAge = race.minAge + Random.nextInt(race.maxAge - race.minAge) / 2
    val bornTurn = turn - currentAge * 4 - Random.nextInt(4)
    val deathAge = currentAge + Random.nextInt(race.maxAge - currentAge)
    val deathTurn = deathAge * 4 + Random.nextInt(4)
    SeasonOfYear.date(bornTurn) -> SeasonOfYear.date(deathTurn)
  }
}