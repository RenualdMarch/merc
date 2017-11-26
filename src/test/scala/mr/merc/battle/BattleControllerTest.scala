package mr.merc.battle

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import mr.merc.map.GameField
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass
import mr.merc.players.Player
import mr.merc.unit.SoldierType
import mr.merc.unit.Attack
import mr.merc.unit.Impact
import mr.merc.unit.Soldier
import mr.merc.map.hex.view.TerrainHexFieldView
import scalafx.beans.property.BooleanProperty
import scalafx.geometry.Rectangle2D
import mr.merc.map.view.SoldiersDrawer

class BattleControllerTest extends FunSuite with BeforeAndAfter {
  var controller: BattleController = _
  val fieldView = new TerrainHexFieldView(new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass)), new SoldiersDrawer(), 1.0)
  val viewRect = new Rectangle2D(0, 0, 100, 100)

  before {
    val hexField = new TerrainHexField(10, 10, (x, y) => new TerrainHex(x, y, Grass))
    val simpleSoldierType = new SoldierType("testType1", 1, 20, 6, 5, 1,
      List(Attack(1, 10, 1, Impact, false)), Map(Grass -> 2),
      Map(Grass -> 60), Map(Impact -> 0))
    val soldier1 = new Soldier("1", simpleSoldierType, Player("1"))
    val soldier2 = new Soldier("2", simpleSoldierType, Player("1"))
    val soldier3 = new Soldier("3", simpleSoldierType, Player("2"))
    val soldier4 = new Soldier("4", simpleSoldierType, Player("2"))
    hexField.hex(0, 0).soldier = Some(soldier1)
    hexField.hex(3, 0).soldier = Some(soldier2)
    hexField.hex(0, 4).soldier = Some(soldier3)
    hexField.hex(7, 4).soldier = Some(soldier4)

    val gameField = new GameField(hexField, List(Player("1"), Player("2")))

    controller = new BattleController(gameField, new BattleControllerParent() {
      def onMinimapChange() {}
      val disableEndTurn = new BooleanProperty()
      def showBattleOverDialog(result: BattleResult) = ???
      def showAttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
        defenderHex: TerrainHex) = ???
    }, 1.0) {
      override def selectAttack(attacker: Soldier, defender: Soldier,
        attackerHex: TerrainHex, defenderHex: TerrainHex) = Some(attacker.soldierType.attacks(0))
    }
  }

  def moveMouse(hexX: Int, hexY: Int) = {
    val center = fieldView.hex(hexX, hexY).center
    val x = center._1
    val y = center._2
    controller.moveMouse(x, y, viewRect)
  }
  def rightClick(hexX: Int, hexY: Int) = {
    moveMouse(hexX, hexY)
    controller.rightClickMouse()
  }

  def leftClick(hexX: Int, hexY: Int) = {
    moveMouse(hexX, hexY)
    controller.leftClickMouse()
  }

  test("on mouse move soldier on hex is shown") {
    moveMouse(3, 0)
    assert(controller.soldierToShow.soldier.get.name === "2")
  }

  test("on mouse move if there is no soldier on hex - nothing is shown") {
    moveMouse(4, 0)
    assert(controller.soldierToShow.soldier === None)
  }

  test("select soldier then move mouse to see shown soldiers") {
    leftClick(3, 0)
    assert(controller.soldierToShow.soldier.get.name === "2")
    moveMouse(0, 0)
    assert(controller.soldierToShow.soldier.get.name === "1")
    moveMouse(1, 1)
    assert(controller.soldierToShow.soldier.get.name === "2")
  }

  test("select soldier then select nothing then select another soldier") {
    leftClick(3, 0)
    assert(controller.selectedSoldier.get.name === "2")
    leftClick(1, 1)
    assert(controller.selectedSoldier === None)
    leftClick(7, 4)
    assert(controller.selectedSoldier.get.name === "4")
  }

  test("select soldier then order him to move") {
    leftClick(0, 0)
    rightClick(1, 0)
    assert(controller.battleView.mapView.terrainView.hex(1, 0).hex.soldier.get.name === "1")
  }

  test("select soldier then move to enemy and see if arrow appears") {
    leftClick(0, 0)
    moveMouse(0, 3)
    moveMouse(0, 4)
    assert(controller.selectedSoldier.get.name === "1")
    assert(controller.soldierToShow.soldier.get.name === "3")
    assert(controller.arrowIsShown === true)
  }

  test("select soldier and see if possible moves are shown") {
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
    assert(controller.movementOptionsAreShown === true)
  }

  test("select soldier and attack enemy") {
    leftClick(0, 0)
    moveMouse(0, 3)
    moveMouse(0, 4)
    assert(controller.selectedSoldier.get.name === "1")
    val attacker = controller.selectedSoldier.get
    assert(controller.soldierToShow.soldier.get.name === "3")
    assert(controller.arrowIsShown === true)
    rightClick(0, 4)
    assert(attacker.attackedThisTurn === true)
    leftClick(0, 3)
    assert(controller.selectedSoldier.get.name === "1")
  }

  test("select soldier and attack unreachable enemy") {
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
    val attacker = controller.selectedSoldier.get
    moveMouse(7, 3)
    rightClick(7, 4)
    assert(attacker.attackedThisTurn === false)
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
  }

  test("select soldier and try to attack another your soldier") {
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
    val attacker = controller.selectedSoldier.get
    moveMouse(2, 0)
    rightClick(3, 0)
    assert(attacker.attackedThisTurn === false)
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
  }

  test("select soldier and try to move to unreachable hex") {
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
    rightClick(7, 7)
    assert(controller.selectedSoldier === None)
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
  }

  test("select soldier and move and see that possible moves are no longer shown and soldier is not selected") {
    leftClick(0, 0)
    assert(controller.selectedSoldier.get.name === "1")
    rightClick(1, 1)
    assert(controller.movementOptionsAreShown === false)
    assert(controller.selectedSoldier === None)
  }

  test("end turn button") {
    assert(controller.battleModel.currentPlayer === Player("1"))
    controller.endTurnButton()
    assert(controller.battleModel.currentPlayer === Player("2"))
    controller.endTurnButton()
    assert(controller.battleModel.currentPlayer === Player("1"))
  }

  test("when click on enemy soldier, moving options are shown") {
    leftClick(7, 4)
    assert(controller.movementOptionsAreShown === true)
  }

  test("when click on enemy soldier and order him to move, you lose selection") {
    leftClick(7, 4)
    rightClick(7, 5)
    assert(controller.selectedSoldier === None)
    assert(controller.movementOptionsAreShown === false)
    leftClick(7, 4)
    assert(controller.selectedSoldier.get.movedThisTurn === false)
    assert(controller.selectedSoldier.get.name === "4")
  }

  test("when click on enemy soldier and order him to move and attack, you lose selection") {
    leftClick(0, 4)
    moveMouse(0, 1)
    rightClick(0, 0)
    assert(controller.selectedSoldier === None)
    leftClick(0, 4)
    assert(controller.selectedSoldier.get.name === "3")
    assert(controller.selectedSoldier.get.movedThisTurn === false)
    assert(controller.selectedSoldier.get.attackedThisTurn === false)
  }

  test("when click on enemy soldier and order hit to attack, you lose selection") {
    val field = controller.battleModel.map.hexField
    val soldier = field.hex(0, 4).soldier.get
    field.hex(0, 4).soldier = None
    field.hex(0, 1).soldier = Some(soldier)
    leftClick(0, 1)
    assert(controller.selectedSoldier.get.name === "3")
    rightClick(0, 0)
    assert(controller.selectedSoldier === None)
    leftClick(0, 1)
    assert(controller.selectedSoldier.get.name === "3")
    assert(controller.selectedSoldier.get.movedThisTurn === false)
    assert(controller.selectedSoldier.get.attackedThisTurn === false)
  }

  test("move points are regenerated after new turn begins") {
    leftClick(0, 0)
    val soldier = controller.selectedSoldier.get
    soldier.movePointsRemain = 0
    soldier.attackedThisTurn = true
    assert(soldier.movedThisTurn === true)
    controller.endTurnButton()
    controller.endTurnButton()
    assert(soldier.movedThisTurn === false)
    assert(soldier.attackedThisTurn === false)
  }

  test("correctly select attack from wrong hex when there is no choice") {
    leftClick(0, 0)
    val attacker = controller.selectedSoldier.get
    rightClick(0, 3)
    assert(controller.battleView.mapView.terrainView.hex(0, 3).hex.soldier.get.name === "1")
    leftClick(0, 3)
    moveMouse(5, 5)
    moveMouse(0, 4)
    assert(controller.arrowIsShown === true)
    assert(attacker.attackedThisTurn === false)
    rightClick(0, 4)
    assert(attacker.attackedThisTurn === true)
  }

  test("soldier is selected and when looking another cell, defence is shown") {
    leftClick(0, 0)
    val selected = controller.selectedSoldier.get
    assert(controller.defenceIsShown === false)
    moveMouse(0, 1)
    assert(controller.defenceIsShown === true)
    moveMouse(1, 0)
    assert(controller.defenceIsShown === true)
  }

  test("soldier is selected and when looking cell out of reach, defence is shown") {
    leftClick(0, 0)
    val selected = controller.selectedSoldier.get
    moveMouse(8, 8)
    assert(controller.defenceIsShown === true)
  }

  test("soldier is selected, when another cell is occupied defence is not shown") {
    leftClick(0, 0)
    val selected = controller.selectedSoldier.get
    moveMouse(3, 0)
    assert(controller.defenceIsShown === false)
    moveMouse(1, 0)
    assert(controller.defenceIsShown === true)
    moveMouse(0, 0)
    assert(controller.defenceIsShown === false)
  }

  test("soldier not selected - defence is not shown") {
    leftClick(1, 0)
    assert(controller.defenceIsShown === false)
    moveMouse(3, 0)
    assert(controller.defenceIsShown === false)
    moveMouse(0, 0)
    assert(controller.defenceIsShown === false)
  }

  test("soldier is deselected - defence is hidden") {
    leftClick(0, 0)
    val selected = controller.selectedSoldier.get
    moveMouse(1, 0)
    assert(controller.defenceIsShown === true)
    rightClick(0, 1)
    assert(controller.defenceIsShown === false)
    assert(controller.selectedSoldier === None)
  }

  test("when enemy soldier selected, defence is shown") {
    leftClick(0, 4)
    val selected = controller.selectedSoldier.get
    moveMouse(1, 0)
    assert(controller.defenceIsShown === true)
    rightClick(0, 1)
    assert(controller.defenceIsShown === false)
    assert(controller.selectedSoldier === None)
  }

  test("when soldier moved, selection is lost and so lost defence") {
    leftClick(0, 0)
    moveMouse(1, 0)
    assert(controller.defenceIsShown === true)
    rightClick(1, 0)
    assert(controller.battleView.mapView.terrainView.hex(1, 0).hex.soldier.get.name === "1")
    assert(controller.defenceIsShown === false)
  }

  test("when soldier attacks, selection is lost and so lost defence") {
    leftClick(0, 0)
    moveMouse(0, 3)
    assert(controller.defenceIsShown === true)
    moveMouse(0, 4)
    assert(controller.selectedSoldier.get.name === "1")
    assert(controller.defenceIsShown === true)
    val attacker = controller.selectedSoldier.get
    assert(controller.soldierToShow.soldier.get.name === "3")
    rightClick(0, 4)
    assert(attacker.attackedThisTurn === true)
    assert(controller.defenceIsShown === false)
  }

  test("when soldier moves and then attacks, defence is not shown") {
    leftClick(0, 0)
    rightClick(0, 3)
    assert(controller.defenceIsShown === false)
    moveMouse(0, 4)
    assert(controller.defenceIsShown === false)
    assert(controller.soldierToShow.soldier.get.name === "3")
  }

  test("when mouse left canvas, defence and arrow are hidden") {
    leftClick(0, 0)
    moveMouse(0, 2)
    assert(controller.defenceIsShown === true)
    controller.mouseLeftCanvas()
    assert(controller.defenceIsShown === false)
    moveMouse(0, 3)
    moveMouse(0, 4)
    assert(controller.arrowIsShown === true)
    controller.mouseLeftCanvas()
    assert(controller.arrowIsShown === false)
  }
}