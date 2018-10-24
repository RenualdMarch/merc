package mr.merc.ai.conditional

import org.scalatest.FunSuite
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit._
import mr.merc.map.hex.TerrainHex
import mr.merc.players.Player
import mr.merc.map.terrain._
import mr.merc.map.GameField
import mr.merc.battle.BattleModel
import org.scalatest.BeforeAndAfter

class AIAgentTest extends FunSuite with BeforeAndAfter {
  val conf = AIConfiguration(0.4, 0, 0, 1, 10, 1)
  val field = new TerrainHexField(10, 10, TerrainHex.grassInit)
  val model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
  val soldierType = new SoldierType("", 20, 40, 4, 1, 1,
    List(new Attack(0, 10, 2, Impact, false)), Map(GrassKind -> 2), Map(GrassDefence -> 50), Map(Impact -> 0))
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

  test("move closer to enemy") {
    def hexInit(x: Int, y: Int) = if (y == 4) new TerrainHex(x, y, DesertSand) else new TerrainHex(x, y, GreenGrass)
    val field = new TerrainHexField(1, 10, hexInit)
    val model = new BattleModel(new GameField(field, List(Player("1"), Player("2"))))
    val soldierType = new SoldierType("", 20, 40, 10, 1, 1,
      List(new Attack(0, 10, 2, Impact, false)), Map(GrassKind -> 2, SandKind -> 2), Map(GrassDefence -> 50, SandDefence -> 60), Map(Impact -> 0))
    val conf1 = AIConfiguration(0.4, 0, 0, 1, 10, 1)
    val soldier = new Soldier("1", soldierType, Player("1"))
    field.hex(0, 0).soldier = Some(soldier)
    val agent1 = new AIAgent(soldier, field.hex(0, 0), conf1)
    val move1 = agent1.moveCloserToEnemy(field.hex(0, 9), model)
    assert(move1.from === field.hex(0, 0))
    assert(move1.to === field.hex(0, 4))
    assert(move1.soldier === soldier)
    val conf0 = AIConfiguration(0.4, 0, 0, 0, 10, 1)
    val agent0 = new AIAgent(soldier, field.hex(0, 0), conf0)
    val move0 = agent0.moveCloserToEnemy(field.hex(0, 9), model)
    assert(move0.from === field.hex(0, 0))
    assert(move0.to === field.hex(0, 5))
    assert(move0.soldier === soldier)
  }
}