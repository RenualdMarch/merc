package mr.merc.ui.battle


import scalafx.scene.layout.VBox
import scalafx.scene.layout.BorderPane
import scalafx.scene.text.Text
import scalafx.animation.Timeline
import scalafx.animation.KeyFrame
import mr.merc.battle.BattleController
import scalafx.animation.Animation
import mr.merc.battle.BattleControllerParent
import mr.merc.unit.Soldier
import mr.merc.map.hex.TerrainHex
import scalafx.scene.control.Button
import scalafx.event.ActionEvent
import scalafx.scene.Node
import mr.merc.ui.minimap.Minimap
import scalafx.geometry.Pos._
import scalafx.scene.layout.GridPane
import mr.merc.local.Localization
import javafx.scene.{input => jfxin}
import mr.merc.ai.BattleAI
import mr.merc.ui.common.SceneManager
import mr.merc.log.Logging
import scalafx.geometry.Rectangle2D
import mr.merc.game.QuickGameGenerator
import mr.merc.battle.BattleResult
import mr.merc.conf.Conf
import mr.merc.map.GameField
import mr.merc.players.Player
import mr.merc.unit.Attack
import scalafx.stage.Modality
import mr.merc.ui.common.CanvasLayers
import mr.merc.ui.world.{BigButton, BigText}

// TODO move all styling to css
class BattleFrame(sceneManager: SceneManager, gameField:GameField, ai:Map[Player, BattleAI], battleOverCallback:() => Unit) extends BorderPane with BattleControllerParent with Logging {

  val scaling = Conf.double("BattleMapScaling")

  import scalafx.Includes._
  val pulse = 30 ms

  val controller = new BattleController(gameField, this, scaling, ai)

  val mapView = controller.battleView.mapView
  private val battleCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight))
  private val minimap = new Minimap(gameField.hexField, battleCanvas, scaling, mapView.pixelWidth, mapView.pixelHeight, true
  )
  private val soldierName = new BigText()
  private val soldierLevel = new BigText()
  private val soldierHP = new BigText()
  private val soldierType = new BigText()
  private val endTurnButton = new BigButton() {
    text = Localization("turn.end")
  }
  override val disableEndTurn = endTurnButton.disable

  private val soldierWrapper = controller.soldierToShow
  private val soldierViewControl = new SoldierViewControl(soldierWrapper, scaling)

  private val rightPanel = new VBox() {
    prefWidth = 400
    maxWidth = 400
    minWidth = 400
    style =
      """
        |  -fx-background-color: white;
        |  -fx-border-color: black;
        |  -fx-border-width: 2;
      """.stripMargin
    spacing = 20
    alignment = TopCenter
    children = List[Node](minimap, soldierViewControl, new GridPane {
      vgap = 20
      hgap = 10
      alignment = TopLeft
      style = "-fx-padding: 0 0 0 100;"
      add(BigText(Localization("soldier.type")), 0, 0)
      add(soldierType, 1, 0)
      add(BigText(Localization("soldier.name")), 0, 1)
      add(soldierName, 1, 1)
      add(BigText(Localization("soldier.level")), 0, 2)
      add(soldierLevel, 1, 2)
      add(BigText(Localization("soldier.hp")), 0, 3)
      add(soldierHP, 1, 3)
    }, endTurnButton)
  }

  center = battleCanvas
  right = rightPanel

  battleCanvas.prefWidth <== this.width - rightPanel.width
  battleCanvas.prefHeight <== this.height
  minimap.prefWidth <== rightPanel.width / 2
  minimap.prefHeight <== rightPanel.width / 2
  soldierName.text <== soldierWrapper.name
  soldierLevel.text <== soldierWrapper.level
  soldierHP.text <== soldierWrapper.hp
  soldierType.text <== soldierWrapper.soldierType

  private def canvasMouseLeft(event: jfxin.MouseEvent) {
    controller.mouseLeftCanvas()
  }

  private def canvasMouseClicked(event: jfxin.MouseEvent) {
    val x = event.getX().toInt
    val y = event.getY().toInt
    controller.moveMouse(x, y, battleCanvas.viewRect)
    if (event.getButton() == jfxin.MouseButton.PRIMARY) {
      controller.leftClickMouse()
    } else if (event.getButton() == jfxin.MouseButton.SECONDARY) {
      controller.rightClickMouse()
    }
  }

  battleCanvas.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_CLICKED, canvasMouseClicked _)
  battleCanvas.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_EXITED, canvasMouseLeft _)

  sceneManager.stage.width.onChange {
    onMinimapChange()
  }

  sceneManager.stage.height.onChange {
    onMinimapChange()
  }

  battleCanvas.onMouseMoved = { event: jfxin.MouseEvent =>
    val x = event.getX().toInt
    val y = event.getY().toInt
    controller.moveMouse(x, y, battleCanvas.viewRect)
  }

  endTurnButton.onAction = { e: ActionEvent => controller.endTurnButton() }

  val timeline = Timeline(KeyFrame(pulse, onFinished = { ev: ActionEvent => gameLoop() }))
  timeline.cycleCount = Animation.Indefinite
  timeline.play()

  var lastUpdateTime = System.currentTimeMillis()
  var battleIsOverDialogShown = false
  def gameLoop() {
    val currentTime = System.currentTimeMillis
    val timePassed = currentTime - lastUpdateTime
    lastUpdateTime = currentTime
    //debug(s"in game loop $timePassed ms passed since previous call")
    if (!battleIsOverDialogShown) {
      controller.update(timePassed.toInt)
      battleCanvas.updateCanvas()
    }
  }

  def onMinimapChange() {
    minimap.redrawScrollCanvas()
    minimap.refreshMapCanvas()
  }

  def showBattleOverDialog(result: BattleResult) {
    battleIsOverDialogShown = true
    val dialog = new BattleResultDialog(result, sceneManager, battleOverCallback)
    dialog.initModality(Modality.WindowModal)
    dialog.initOwner(sceneManager.stage)
    dialog.centerOnScreen()
    dialog.show()
  }

  this.onScroll = { event =>
    val delta = event.getDeltaY
    val diff = delta * 7
    val d = diff / mapView.pixelHeight
    val v = keepHValueInBounds(battleCanvas.vvalue.value - d)
    battleCanvas.vvalue.value = v
  }

  private def keepHValueInBounds(hv: Double): Double = {
    if (hv < 0) 0
    else if (hv > 1) 1
    else hv
  }

  def showAttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): Option[Attack] = {

    val dialog = new AttackSelectionDialog(attacker, defender, attackerHex, defenderHex)

    dialog.initModality(Modality.WindowModal)
    dialog.initOwner(sceneManager.stage.delegate)
    dialog.centerOnScreen()
    dialog.showAndWait()

    dialog.selectedAttack
  }
}