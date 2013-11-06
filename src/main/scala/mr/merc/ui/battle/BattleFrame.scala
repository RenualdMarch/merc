package mr.merc.ui.battle


import javafx.scene.{layout => jfxsl}
import javafx.scene.{text => jfxt}
import javafx.{fxml => jfxf}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.{canvas => jfxc}
import javafx.scene.{image => jfxi}
import javafx.{event => jfxe}
import javafx.scene.{input => jfxin}
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

class BattleFrame extends jfxf.Initializable with BattleControllerParent {
  val field = new TerrainHexField(5, 5, mapInit)
  val player1 = Player("1", Color.BLUE)
  val player2 = Player("2", Color.YELLOW)
  val soldier = new Soldier("1", SoldierType("Human-Horseman"), player1)
  soldier.exp = 10
  val enemy = new Soldier("2", SoldierType("Human-Horseman"), player2)
  field.hex(4, 1).soldier = Some(soldier)
  field.hex(4, 3).soldier = Some(enemy)
  val gameField = new GameField(field, List(player1, player2))
  private def mapInit(x:Int, y:Int) = 
    if (x == 1 || x == 2) {
      new TerrainHex(x, y, Water, if (y == 2) Some(WoodenBridge) else None)
    } else if (y == 2){
      new TerrainHex(x, y, Forest)
    } else if (x == 4 && y == 3){
      new TerrainHex(x, y, Grass, Some(House))
    } else {
      new TerrainHex(x, y, Sand)
    }
  
  
  
  val controller = new BattleController(gameField, this)
  
  @jfxf.FXML
  private var rightPanelDelegate: jfxsl.VBox = _
  private var rightPanel: VBox = _
  
  @jfxf.FXML
  private var battleCanvasDelegate: jfxc.Canvas = _
  private var battleCanvas:Canvas = _
  
  @jfxf.FXML
  private var parentPaneDelegate: jfxsl.BorderPane = _
  private var parentPane:BorderPane = _
  
  @jfxf.FXML
  private var miniMapCanvasDelegate: jfxc.Canvas = _
  private var miniMapCanvas: Canvas = _
  
  @jfxf.FXML
  private var soldierImageViewDelegate: jfxi.ImageView = _
  private var soldierImageView : ImageView = _
  
  @jfxf.FXML
  private var soldierNameDelegate: jfxt.Text = _
  private var soldierName : Text = _
  
  @jfxf.FXML
  private var soldierLevelDelegate: jfxt.Text = _
  private var soldierLevel : Text = _
  
  @jfxf.FXML
  private var soldierHPDelegate: jfxt.Text = _
  private var soldierHP : Text = _
  
  private var soldierWrapper = controller.soldierToShow
  
  private def gc = battleCanvas.graphicsContext2D
  
  def initialize(url: URL, rb: ResourceBundle) {
    rightPanel = new VBox(rightPanelDelegate)
    battleCanvas = new Canvas(battleCanvasDelegate)
    parentPane = new BorderPane(parentPaneDelegate)
    miniMapCanvas = new Canvas(miniMapCanvasDelegate)
    soldierName = new Text(soldierNameDelegate)
    soldierLevel = new Text(soldierLevelDelegate)
    soldierHP = new Text(soldierHPDelegate)
    
    battleCanvas.width <== parentPane.width - rightPanel.width
    battleCanvas.height <== parentPane.height
    soldierName.text <== soldierWrapper.name
    soldierLevel.text <== soldierWrapper.level
    soldierHP.text <== soldierWrapper.hp
    
    // Yeah, java style
    battleCanvasDelegate.addEventHandler(jfxin.MouseEvent.MOUSE_CLICKED, new jfxe.EventHandler[jfxin.MouseEvent] {
      def handle(event:jfxin.MouseEvent) {
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
    
    battleCanvasDelegate.addEventHandler(jfxin.MouseEvent.MOUSE_MOVED, new jfxe.EventHandler[jfxin.MouseEvent] {
      def handle(event:jfxin.MouseEvent) {
        val x = event.getX().toInt
        val y = event.getY().toInt
        controller.moveMouse(x, y)
      }            
    })
    
    import scalafx.Includes._
    val timeline = Timeline(KeyFrame(50 ms, onFinished = gameLoop))
    timeline.cycleCount = Animation.INDEFINITE
    timeline.play()
  }
  
  
  private def reset(color: Color) {
    gc.fill = color
    gc.fillRect(0, 0, battleCanvas.width.get, battleCanvas.height.get);    
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
}