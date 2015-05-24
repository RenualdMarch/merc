package mr.merc.person

import java.time.LocalDate

import org.scalatest.FunSuite

class PsychoTest extends FunSuite {
  test("sociotypes relations are symmetric") {
    for (_ <- 0 until 100) {
      val f = SocioType.randomType
      val s = SocioType.randomType
      assert(f.relations(s) === s.relations(f), s"Error for $f and $s")
    }
  }

  test("morale after victory") {
    val morale = new Morale(0.5)
    for (_ <- 0 until 10) {
      val moralePrev = morale.moraleLevel
      morale.win(0.5)
      assert(morale.moraleLevel > moralePrev)
      assert(morale.moraleLevel <= 1)
    }
  }

  test("morale after defeat") {
    val morale = new Morale(0.5)
    for (_ <- 0 until 10) {
      val moralePrev = morale.moraleLevel
      morale.lost(0.5)
      assert(morale.moraleLevel <= moralePrev)
      assert(morale.moraleLevel >= 0)
    }
  }

  test("friendship is magic") {
    val p1 = new Person(PersonName("1", "2", "3"), LocalDate.now(), new Psycho(SocioType(false, true, true, true)), new Exp(0.5, 100), UKR)
    val p2 = new Person(PersonName("4", "5", "6"), LocalDate.now(), new Psycho(SocioType(true, false, false, true)), new Exp(0.5, 100), UKR)

    assert(p1.psycho.socioType.relations(p2.psycho.socioType) === SocioType.Dual)
    for (_ <- 0 until 10) {
      p1.psycho.refreshRelationships(p1, p2)
    }

    assert(p1.psycho.friends.contains(p2))
    assert(p2.psycho.friends.contains(p1))
    assert(p1.psycho.enemies.isEmpty)
    assert(p2.psycho.enemies.isEmpty)
  }

  test("enemy mine") {
    val p1 = new Person(PersonName("1", "2", "3"), LocalDate.now(), new Psycho(SocioType(false, true, true, true)), new Exp(0.5, 100), UKR)
    val p2 = new Person(PersonName("4", "5", "6"), LocalDate.now(), new Psycho(SocioType(true, false, false, false)), new Exp(0.5, 100), UKR)

    assert(p1.psycho.socioType.relations(p2.psycho.socioType) === SocioType.Conflict)
    for (_ <- 0 until 10) {
      p1.psycho.refreshRelationships(p1, p2)
    }

    assert(p1.psycho.enemies.contains(p2))
    assert(p2.psycho.enemies.contains(p1))
    assert(p1.psycho.friends.isEmpty)
    assert(p2.psycho.friends.isEmpty)
  }

}
