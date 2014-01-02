package mr.merc.ai.conditional

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit.SoldierType
import mr.merc.map.hex.TerrainHex
import mr.merc.unit.Soldier
import mr.merc.players.Player
import mr.merc.unit.Attack
import mr.merc.unit.Impact
import mr.merc.map.terrain.Grass
import mr.merc.map.GameField
import mr.merc.battle.BattleModel
import org.scalatest.BeforeAndAfter

class AIAgentTest extends FunSuite with BeforeAndAfter {
  val conf = AIConfiguration(0.4)
  val field = new TerrainHexField(10, 10, TerrainHex.grassInit)
  val model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
  val soldierType = new SoldierType("", 20, 40, 4, 1, 1,
    List(new Attack(0, 10, 2, Impact, false)), Map(Grass -> 2), Map(Grass -> 50), Map(Impact -> 0))
  val attacker = new Soldier("1", soldierType, Player("1"))
  val defender1 = new Soldier("1", soldierType, Player("2"))
  val defender2 = new Soldier("1", soldierType, Player("2"))

  before {
    model.map.hexField.hexes.foreach(_.soldier = None)
  }

  test("best target selection for unit without moving") {
    import model.map.hexField._
    hex(0, 0).soldier = Some(attacker)
    hex(0, 1).soldier = Some(defender1)
    hex(1, 0).soldier = Some(defender2)
    defender2.hp = 10
    val agent = new AIAgent(attacker, hex(0, 0), conf)
    val bestTarget = agent.bestTarget(model)
    defender2.hp = 20
    assert(bestTarget.isDefined === true)
    assert(bestTarget.get.hexFromWhichAttack === hex(0, 0))
    assert(bestTarget.get.hexWhichAttack === hex(1, 0))
  }

  test("best target selection for unit with moving") {
    import model.map.hexField._
    hex(0, 0).soldier = Some(attacker)
    hex(0, 3).soldier = Some(defender1)
    hex(3, 0).soldier = Some(defender2)
    defender1.hp = 10
    val agent = new AIAgent(attacker, hex(0, 0), conf)
    val bestTarget = agent.bestTarget(model)
    assert(bestTarget.isDefined === true)
    assert(bestTarget.get.hexFromWhichAttack === hex(0, 2))
    assert(bestTarget.get.hexWhichAttack === hex(0, 3))
  }

  test("best target selection when there are no enemies near") {
    import model.map.hexField._
    hex(0, 0).soldier = Some(attacker)
    val agent = new AIAgent(attacker, hex(0, 0), conf)
    val bestTarget = agent.bestTarget(model)
    assert(bestTarget === None)
  }
}