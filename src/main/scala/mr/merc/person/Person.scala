package mr.merc.person

import java.time.LocalDate

case class Person(name: PersonName, born:LocalDate, exp: Exp, soldierClass: String) {

}

case class PersonName(name: String, surname: String, nick: String)

class Exp(talent: Double, initialPoints: Int = 0) {
  private val formula = new LevelUpFormula(x => 1000 * x * x)
  private val mechanic = new LevelUpMechanic(initialPoints, formula)
  def level = mechanic.level
  def pointsToNextLevel = formula.pointsToLevel(points)
  def pointsInThisLevel = formula.pointsAfterThisLevel(points)
  def points = mechanic.points
  def addPoints(points: Double): Unit = {
    val withTalent = points * (1 + talent)
    mechanic.addPoints(withTalent)
  }
}

