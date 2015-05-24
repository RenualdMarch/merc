package mr.merc.person

import org.scalatest.FunSuite

class LevelUpTest extends FunSuite {
  test("LevelUpFormula") {
    val formula = new LevelUpFormula(x => x * 10)
    assert(formula.pointsToLevel(15) === 1)
    assert(formula.pointsToLevel(40) === 4)
    assert(formula.pointsToLevel(10001) === 1000)

    assert(formula.pointsAfterThisLevel(15) === 5)
    assert(formula.pointsAfterThisLevel(50) === 0)

    assert(formula.remainToNextLevel(15) === 5)
    assert(formula.remainToNextLevel(50) === 10)
  }

  test("LevelUpMechanic") {
    val mech = new LevelUpMechanic(10, new LevelUpFormula(x => x * 10))
    assert(mech.level === 1)
    assert(mech.points === 10)

    mech.addPoints(20)

    assert(mech.level === 3)
    assert(mech.points === 30)
  }
}
