package mr.merc.person

import mr.merc.map.terrain.{Mountain, Grass}
import mr.merc.unit._
import org.scalatest.FunSuite

class PersonSoldierTypeTest extends FunSuite {
  val inf = PersonSoldierClass("st")

  test("exact numbers") {
    assert(inf.hp(0) === 30)
    assert(inf.hp(20) === 50)
    assert(inf.hp(40) === 90)


    assert(inf.attacks(0) === List(Attack(0, 10, 2, Impact, ranged = false)))
    assert(inf.attacks(20) === List(Attack(0, 15, 2, Impact, ranged = false), Attack(1, 30, 1, Pierce, ranged = true, attributes = Set(Magical))))
    assert(inf.attacks(40) === List(Attack(0, 20, 3, Impact, ranged = false), Attack(1, 10, 3, Blade, ranged = false)))

    assert(inf.attributes(0) === Set())
    assert(inf.attributes(20) === Set(Skirmisher))
    assert(inf.attributes(40) === Set())

    assert(inf.cost(0) === 10)
    assert(inf.cost(20) === 20)
    assert(inf.cost(40) === 30)

    assert(inf.defence(0)(Grass) === 30)
    assert(inf.defence(20)(Grass) === 40)
    assert(inf.defence(40)(Grass) === 50)

    assert(inf.moveCost(0)(Mountain) === 4)
    assert(inf.moveCost(20)(Mountain) === 3)
    assert(inf.moveCost(40)(Mountain) === 2)

    assert(inf.resistance(0)(Blade) === 30)
    assert(inf.resistance(20)(Blade) === 40)
    assert(inf.resistance(40)(Blade) === 50)
  }

  test("numbers between") {
    assert(inf.hp(10) === 40)
    assert(inf.hp(30) === 70)
    assert(inf.hp(50) === 110)


    assert(inf.attacks(5) === List(Attack(0, 10, 2, Impact, ranged = false)))
    assert(inf.attacks(25) === List(Attack(0, 15, 2, Impact, ranged = false), Attack(1, 30, 1, Pierce, ranged = true, attributes = Set(Magical))))
    assert(inf.attacks(45) === List(Attack(0, 20, 3, Impact, ranged = false), Attack(1, 10, 3, Blade, ranged = false)))

    assert(inf.attributes(5) === Set())
    assert(inf.attributes(25) === Set(Skirmisher))
    assert(inf.attributes(45) === Set())

    assert(inf.cost(10) === 15)
    assert(inf.cost(30) === 25)
    assert(inf.cost(50) === 35)

    assert(inf.defence(5)(Grass) === 30)
    assert(inf.defence(25)(Grass) === 40)
    assert(inf.defence(45)(Grass) === 50)

    assert(inf.moveCost(5)(Mountain) === 4)
    assert(inf.moveCost(25)(Mountain) === 3)
    assert(inf.moveCost(45)(Mountain) === 2)

    assert(inf.resistance(5)(Blade) === 30)
    assert(inf.resistance(25)(Blade) === 40)
    assert(inf.resistance(45)(Blade) === 50)
  }
}
