package mr.merc.ui.battle

import scalafx.Includes._
import javafx.scene.{layout => jfxsl}
import javafx.scene.{text => jfxt}
import javafx.{fxml => jfxf}
import java.net.URL
import java.util.ResourceBundle
import javafx.scene.{canvas => jfxc}
import javafx.scene.{image => jfxi}
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
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.scene.image.ImageView
import scalafx.scene.text.Text
import javafx.{fxml => jfxf}
import javafx.scene.{canvas => jfxc}
import javafx.scene.{image => jfxi}
import javafx.scene.{layout => jfxsl}
import javafx.scene.{text => jfxt}
import scalafx.beans.binding.NumberBinding.sfxNumberBinding2jfx
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.beans.property.ReadOnlyDoubleProperty.sfxReadOnlyDoubleProperty2jfx
import mr.merc.ui.common.SoldierWrapper
import mr.merc.ui.common.SoldierWrapper

class BattleController extends jfxf.Initializable {

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
  
  private var soldierWrapper: SoldierWrapper = _
  
  val field = new TerrainHexField(5, 5, mapInit)
  val soldier = new Soldier("1", SoldierType("Human-Horseman"), Player(""))
  field.hex(4, 1).soldier = Some(soldier)
  val mapView = new MapView(field)
  
  private def gc = battleCanvas.graphicsContext2D
  
  def initialize(url: URL, rb: ResourceBundle) {
    rightPanel = new VBox(rightPanelDelegate)
    battleCanvas = new Canvas(battleCanvasDelegate)
    parentPane = new BorderPane(parentPaneDelegate)
    miniMapCanvas = new Canvas(miniMapCanvasDelegate)
    soldierName = new Text(soldierNameDelegate)
    soldierLevel = new Text(soldierLevelDelegate)
    soldierHP = new Text(soldierHPDelegate)
    
    soldierWrapper = new SoldierWrapper(soldier)
    
    battleCanvas.width <== parentPane.width - rightPanel.width
    battleCanvas.height <== parentPane.height
    soldierName.text <== soldierWrapper.name
    soldierLevel.text <== soldierWrapper.level
    soldierHP.text <== soldierWrapper.hp
    
    val timeline = Timeline(KeyFrame(Duration(50), "baseLoop", gameLoop()))
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