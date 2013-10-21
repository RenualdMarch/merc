package mr.merc.battle

import scalafx.Includes._
import javafx.scene.{layout => jfxsl}
import javafx.{fxml => jfxf}
import java.net.URL
import java.util.ResourceBundle
import scalafx.scene.layout.GridPane
import javafx.scene.{canvas => jfxc}
import scalafx.scene.canvas.Canvas
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit.Soldier
import mr.merc.map.view.MapView
import mr.merc.players.Player
import mr.merc.unit.SoldierType
import scalafx.animation.Timeline
import scalafx.animation.KeyFrame
import scalafx.scene.paint.Color
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._
import scalafx.util.Duration
import scalafx.animation.Animation
import scalafx.stage.Screen
import scalafx.scene.layout.BorderPane

class BattleController extends jfxf.Initializable {

  @jfxf.FXML
  private var rightPanelDelegate: jfxsl.GridPane = _
  private var rightPanel: GridPane = _
  
  @jfxf.FXML
  private var battleCanvasDelegate: jfxc.Canvas = _
  private var battleCanvas:Canvas = _
  
  @jfxf.FXML
  private var parentPaneDelegate: jfxsl.BorderPane = _
  private var parentPane:BorderPane = _
  
  private def gc = battleCanvas.graphicsContext2D
  
  def initialize(url: URL, rb: ResourceBundle) {
    rightPanel = new GridPane(rightPanelDelegate)
    battleCanvas = new Canvas(battleCanvasDelegate)
    parentPane = new BorderPane(parentPaneDelegate)
    
    battleCanvas.width <== parentPane.width - rightPanel.width
    battleCanvas.height <== parentPane.height
    
        
    val timeline = Timeline(KeyFrame(Duration(50), "baseLoop", gameLoop()))
    timeline.cycleCount = Animation.INDEFINITE
    timeline.play()
  }
  
  val field = new TerrainHexField(5, 5, mapInit)
  val soldier = new Soldier("1", SoldierType("Human-Horseman"), Player(""))
  field.hex(4, 1).soldier = Some(soldier)
  val mapView = new MapView(field)
  
  private def reset(color: Color) {
    gc.fill = color
    gc.fillRect(0, 0, battleCanvas.width.get, battleCanvas.height.get);    
  }  
  
  var lastUpdateTime = System.currentTimeMillis()
  def gameLoop() {
    val currentTime = System.currentTimeMillis
    val timePassed = currentTime - lastUpdateTime
    lastUpdateTime = currentTime
    mapView.update(timePassed.toInt)
    reset(Color.BLUE)
    mapView.drawItself(gc)
  }
  
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

}