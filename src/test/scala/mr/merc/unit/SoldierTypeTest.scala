package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.map.terrain.{GrassKind, SandKind}

class SoldierTypeTest extends FunSuite {
  test("parsing") {
    val sType = SoldierType("testSoldier")
    assert(sType.cost === 2)
    assert(sType.hp === 3)
    assert(sType.movement === 10)
    assert(sType.name === "testSoldier")
    assert(sType.exp === 45)
    assert(sType.level === 3)
    assert(sType.attacks(1).attributes === Set(Drain, Firststrike))
    assert(sType.attributes === Set.empty)

    val attacks = sType.attacks
    assert(attacks.size === 2)
    val attack0 = attacks(0)
    assert(attack0.attackType === Impact)
    assert(attack0.count === 4)
    assert(attack0.damage === 3)
    assert(attack0.ranged === false)
    assert(attack0.index === 0)
    val attack1 = attacks(1)
    assert(attack1.attackType === Fire)
    assert(attack1.count === 3)
    assert(attack1.damage === 2)
    assert(attack1.ranged === true)
    assert(attack1.index === 1)
    assert(sType.moveCost(SandKind) === 1)
    assert(sType.moveCost(GrassKind) === 2)

    assert(sType.defence(SandDefence) === 50)
    assert(sType.defence(GrassDefence) === 30)

    assert(sType.resistance(Impact) === 0)
    assert(sType.resistance(Pierce) === 20)
    assert(sType.resistance(Arcane) === -30)
    assert(sType.resistance(Blade) === 0)
    assert(sType.resistance(Cold) === 0)
    assert(sType.resistance(Fire) === 0)

    val sType2 = SoldierType("testSoldier2")
    assert(sType2.attributes === Set(Cures, Heals4))
    assert(sType2.attacks(0).attributes === Set(Poison))
  }
}