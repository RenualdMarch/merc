package mr.merc.ui.battle

import java.net.URL
import java.util.ResourceBundle
import scalafx.scene.layout.VBox
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.BorderPane
import scalafx.scene.image.ImageView
import scalafx.scene.text.Text
import mr.merc.ui.common.SoldierWrapper
import scalafx.animation.Timeline
import scalafx.animation.KeyFrame
import mr.merc.battle.BattleController
import scalafx.scene.paint.Color
import scalafx.util.Duration
import scalafx.animation.Animation
import mr.merc.battle.BattleControllerParent
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit.Soldier
import mr.merc.map.GameField
import mr.merc.players.Player
import mr.merc.unit.SoldierType
import mr.merc.map.hex.TerrainHex
import mr.merc.map.objects._
import mr.merc.map.terrain._
import scalafx.scene.control.Button
import javafx.event.ActionEvent
import scalafx.scene.Node
import scalafx.stage.Window
import mr.merc.ui.minimap.Minimap
import scalafx.geometry.Pos._
import scalafx.scene.layout.GridPane
import mr.merc.local.Localization
import javafx.{ event => jfxe }
import javafx.scene.{ input => jfxin }
import mr.merc.ui.common.ConversionUtils._
import mr.merc.ui.common.SceneManager
import scalafx.scene.control.ScrollPane
import mr.merc.map.generator.RandomTerrainGenerator
import mr.merc.log.Logging
import scalafx.scene.CacheHint
import scalafx.geometry.Rectangle2D
import mr.merc.game.QuickGameGenerator
import mr.merc.battle.BattleResult
import mr.merc.unit.Attack
import scalafx.stage.Modality
import mr.merc.ui.common.CanvasLayer

// TODO move all styling to css
class BattleFrame(sceneManager: SceneManager) extends BorderPane with BattleControllerParent with Logging {

  import scalafx.Includes._
  val pulse = 20 ms
  val gameField = new QuickGameGenerator().generateGame

  val controller = new BattleController(gameField, this)

  val mapView = controller.battleView.mapView
  private val battleCanvas = new CanvasLayer(4, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight), controller.cleanRedraw)
  private val minimap = new Minimap(gameField.hexField, battleCanvas)
  private val soldierName = new Text()
  private val soldierLevel = new Text()
  private val soldierHP = new Text()
  private val soldierType = new Text()
  private val endTurnButton = new Button() {
    text = Localization("turn.end")
  }
  override val disableEndTurn = endTurnButton.disable

  private val soldierWrapper = controller.soldierToShow
  private val soldierViewControl = new SoldierViewControl(soldierWrapper)

  private val rightPanel = new VBox() {
    prefWidth = 400
    style = "-fx-background-color: cyan"
    spacing = 20
    alignment = TOP_CENTER
    content = List[Node](minimap, soldierViewControl, new GridPane {
      vgap = 20
      hgap = 10
      alignment = TOP_LEFT
      style = "-fx-padding: 0 0 0 100;"
      add(new Text {
        text = Localization("soldier.type")
      }, 0, 0)
      add(soldierType, 1, 0)
      add(new Text {
        text = Localization("soldier.name")
      }, 0, 1)
      add(soldierName, 1, 1)
      add(new Text {
        text = Localization("soldier.level")
      }, 0, 2)
      add(soldierLevel, 1, 2)
      add(new Text {
        text = Localization("soldier.hp")
      }, 0, 3)
      add(soldierHP, 1, 3)
    }, endTurnButton)
  }

  // Initialization
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

  val timeline = Timeline(KeyFrame(pulse, onFinished = gameLoop))
  timeline.cycleCount = Animation.INDEFINITE
  timeline.play()

  var lastUpdateTime = System.currentTimeMillis()
  var battleIsOverDialogShown = false
  def gameLoop() {
    val currentTime = System.currentTimeMillis
    val timePassed = currentTime - lastUpdateTime
    lastUpdateTime = currentTime
    debug(s"in game loop $timePassed ms passed since previous call")
    if (!battleIsOverDialogShown) {
      controller.update(timePassed.toInt)
      controller.updateBattleCanvas(battleCanvas)
    }
  }

  def onMinimapChange() {
    minimap.redraw()
  }

  def showBattleOverDialog(result: BattleResult) {
    battleIsOverDialogShown = true
    val dialog = new BattleResultDialog(result, sceneManager)
    dialog.initModality(Modality.WINDOW_MODAL)
    dialog.initOwner(sceneManager.stage)
    dialog.centerOnScreen()
    dialog.show()
  }

  def showAttackSelectionDialog(attacker: Soldier, defender: Soldier, attackerHex: TerrainHex,
    defenderHex: TerrainHex): Option[Attack] = {

    val dialog = new AttackSelectionDialog(attacker, defender, attackerHex, defenderHex)

    dialog.initModality(Modality.WINDOW_MODAL)
    dialog.initOwner(sceneManager.stage.delegate)
    dialog.centerOnScreen()
    dialog.showAndWait()

    dialog.selectedAttack
  }
}