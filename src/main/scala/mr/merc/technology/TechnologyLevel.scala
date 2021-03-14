package mr.merc.technology

import mr.merc.economics.WorldConstants

import scala.annotation.tailrec

class TechnologyLevel(private var currentLevel:Int) {

  private var currentLevelPoints: Double = 0d

  def pointsForNextLevel: Double = WorldConstants.Technology.pointsToLevel(currentLevel + 1)

  def technologyLevel:Int = currentLevel

  def pointsInThisLevel: Double = currentLevelPoints

  def pointsToTheNextLevel: Double = pointsForNextLevel - pointsInThisLevel

  def addPoints(averageLiteracy: Double, acceptedScholarsCount: Int): Unit = {

    @tailrec
    def appendPoints(append: Double): Unit = {
      if (append < pointsToTheNextLevel) {
        currentLevelPoints += append
      } else {
        val remainingPoints = append - pointsToTheNextLevel
        currentLevel += 1
        appendPoints(remainingPoints)
      }
    }

    val newPoints = WorldConstants.Technology.pointsForTurn(averageLiteracy, acceptedScholarsCount)
    appendPoints(newPoints)
  }

}
