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

class BattleFrame extends BattleControllerParent {
  val field = new TerrainHexField(5, 5, mapInit)
  val player1 = Player("1", Color.BLUE)
  val player2 = Player("2", Color.YELLOW)
  val soldier = new Soldier("1", SoldierType("Human-Horseman"), player2)
  soldier.exp = 10
  val enemy = new Soldier("2", SoldierType("Human-Mage"), player1)
  field.hex(4, 1).soldier = Some(soldier)
  field.hex(4, 3).soldier = Some(enemy)
  val gameField = new GameField(field, List(player1, player2))
  private def mapInit(x: Int, y: Int) =
    if (x == 1 || x == 2) {
      new TerrainHex(x, y, Water, if (y == 2) Some(WoodenBridge) else None)
    } else if (y == 2) {
      new TerrainHex(x, y, Forest)
    } else if (x == 4 && y == 3) {
      new TerrainHex(x, y, Grass, Some(House))
    } else {
      new TerrainHex(x, y, Sand)
    }

  val controller = new BattleController(gameField, this)

  override def window = battleCanvas.asInstanceOf[Node].getScene().getWindow()

  private val battleCanvas = new Canvas()
  private val minimap = new Minimap(field)
  private val soldierImageView = new ImageView()
  private val soldierName = new Text()
  private val soldierLevel = new Text()
  private val soldierHP = new Text()
  private val endTurnButton = new Button() {
    text = Localization("turn.end")
  }

  private val soldierWrapper = controller.soldierToShow

  private def gc = battleCanvas.graphicsContext2D

  private val rightPanel = new VBox() {
    prefWidth = 400
    style = "-fx-background-color: cyan"
    spacing = 20
    alignment = TOP_CENTER
    content = List[Node](minimap, soldierImageView, new GridPane {
      vgap = 20
      hgap = 10
      alignment = TOP_LEFT
      style = "-fx-padding: 0 0 0 100;"
      add(new Text {
        text = Localization("soldier.name")
      }, 0, 0)
      add(soldierName, 1, 0)
      add(new Text {
        text = Localization("soldier.level")
      }, 0, 1)
      add(soldierLevel, 1, 1)
      add(new Text {
        text = Localization("soldier.hp")
      }, 0, 2)
      add(soldierHP, 1, 2)
    }, endTurnButton)
  }

  val parentPane = new BorderPane() {
    center = battleCanvas
    right = rightPanel
  }

  battleCanvas.width <== parentPane.width - rightPanel.width
  battleCanvas.height <== parentPane.height
  minimap.prefWidth <== rightPanel.width / 2
  minimap.prefHeight <== rightPanel.width / 2
  soldierName.text <== soldierWrapper.name
  soldierLevel.text <== soldierWrapper.level
  soldierHP.text <== soldierWrapper.hp

  // TODO remove java style here and below
  battleCanvas.delegate.addEventHandler(jfxin.MouseEvent.MOUSE_CLICKED, new jfxe.EventHandler[jfxin.MouseEvent] {
    def handle(event: jfxin.MouseEvent) {
      val x = event.getX().toInt
      val y = event.getY().toInt
      controller.moveMouse(x, y)
      if (event.getButton() == jfxin.MouseButton.PRIMARY) {
        controller.leftClickMouse()
      } else if (event.getButton() == jfxin.MouseButton.SECONDARY) {
        controller.rightClickMouse()
      }
    }
  })

  battleCanvas.onMouseMoved = new jfxe.EventHandler[jfxin.MouseEvent] {
    def handle(event: jfxin.MouseEvent) {
      val x = event.getX().toInt
      val y = event.getY().toInt
      controller.moveMouse(x, y)
    }
  }

  endTurnButton.onAction = new jfxe.EventHandler[ActionEvent] {
    def handle(event: ActionEvent) {
      controller.endTurnButton()
    }
  }

  import scalafx.Includes._
  val timeline = Timeline(KeyFrame(50 ms, onFinished = gameLoop))
  timeline.cycleCount = Animation.INDEFINITE
  timeline.play()

  private def reset(color: Color) {
    gc.save()
    gc.fill = color
    gc.fillRect(0, 0, battleCanvas.width.get, battleCanvas.height.get);
    gc.restore()
  }

  var lastUpdateTime = System.currentTimeMillis()
  def gameLoop() {
    val currentTime = System.currentTimeMillis
    val timePassed = currentTime - lastUpdateTime
    lastUpdateTime = currentTime
    controller.update(timePassed.toInt)
    reset(Color.BLUE)
    controller.drawBattleCanvas(gc)
  }

  def onMinimapChange() {
    minimap.redraw()
  }
}