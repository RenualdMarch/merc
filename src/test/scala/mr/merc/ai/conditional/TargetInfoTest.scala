package mr.merc.ai.conditional

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.SoldierType
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.ai.AttackResultPrediction
import mr.merc.unit.Attack
import mr.merc.unit.AttackType._

class TargetInfoTest extends FunSuite {
  val conf = AIConfiguration(0.65, 0, 0, 0, 10, 1)
  val field = new TerrainHexField(10, 10, TerrainHex.grassInit)
  def soldierType(cost: Int) = new SoldierType("", cost, 10, 0, 1, 1,
    List(new Attack(0, 10, 2, Impact, false)), Map(), Map(), Map(), viewName = "")
  val attacker = new Soldier("1", soldierType(10), Player("1"))
  val defender1 = new Soldier("1", soldierType(10), Player("2"))
  val defender2 = new Soldier("1", soldierType(11), Player("2"))
  field.hex(0, 0).soldier = Some(attacker)
  field.hex(0, 1).soldier = Some(defender1)
  field.hex(1, 0).soldier = Some(defender2)
  val attack = attacker.soldierType.attacks(0)

  val agent = new AIAgent(attacker, field.hex(0, 0), conf)

  test("killing possibility is better that without it") {
    val better = agent.TargetInfo(field.hex(0, 0), field.hex(0, 1), attack, AttackResultPrediction(0.1, 0.7, 10, 10))
    val worse = agent.TargetInfo(field.hex(0, 0), field.hex(1, 0), attack, AttackResultPrediction(0.1, 0.6, 10, 10))
    assert(better > worse === true)
    assert(worse < better === true)
  }

  test("when possible to kill both enemies, must choose those who is more expensive") {
    val better = agent.TargetInfo(field.hex(0, 0), field.hex(1, 0), attack, AttackResultPrediction(0.1, 0.8, 10, 10))
    val worse = agent.TargetInfo(field.hex(0, 0), field.hex(0, 1), attack, AttackResultPrediction(0.1, 0.8, 10, 10))
    assert(better > worse === true)
    assert(worse < better === true)
  }

  test("when not possible to kill, must choose those where damage done/received ratio is bigger") {
    val better = agent.TargetInfo(field.hex(0, 0), field.hex(1, 0), attack, AttackResultPrediction(0.1, 0.5, 5, 10))
    val worse = agent.TargetInfo(field.hex(0, 0), field.hex(0, 1), attack, AttackResultPrediction(0.1, 0.5, 7, 10))
    assert(better > worse === true)
    assert(worse < better === true)

  }

  test("prev test can work with zeros") {
    val better = agent.TargetInfo(field.hex(0, 0), field.hex(1, 0), attack, AttackResultPrediction(0.1, 0.5, 0, 10))
    val worse = agent.TargetInfo(field.hex(0, 0), field.hex(0, 1), attack, AttackResultPrediction(0.1, 0.5, 0, 5))
    assert(better > worse === true)
    assert(worse < better === true)
  }
}