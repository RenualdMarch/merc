package mr.merc.ui.world

import javafx.event.EventHandler
import mr.merc.economics.WorldGenerator
import mr.merc.log.Logging
import mr.merc.map.hex.view.ProvinceView
import mr.merc.map.hex.view.TerrainHexFieldView.WorldMapViewMode
import mr.merc.map.view.MapView
import mr.merc.politics.{Province, State}
import mr.merc.ui.common.{CanvasLayers, SceneManager}
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.Includes._
import javafx.scene.input.{KeyEvent => JKeyEvent}
import javafx.scene.input.{KeyCode => JKeyCode}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object WorldFrame {
  val PixelsPerScroll = 200
}

class WorldFrame(sceneManager: SceneManager) extends Pane with Logging {
  val factor = 1d

  val worldState = WorldGenerator.generateWorld()
  val mapView = new MapView(worldState.worldHexField, factor, mode = WorldMapViewMode)
  val menu = new WorldMenu(this)
  val stateLabel:Pane = new PlayerStateData(worldState.playerState)

  val provinceViews = worldState.states.flatMap { case (_, pList) =>
    pList.map { p =>
      new ProvinceView(p, worldState.worldHexField, mapView.terrainView)
    }
  }
  val provinceViewsMap = provinceViews.map(p => p.province -> p).toMap
  provinceViews.foreach(_.refreshCity())
  provinceViews.foreach(_.refreshSoldiers())
  mapView.terrainView.refreshTerrainDirt()
  private val worldCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight))

  private val interfacePane = new WorldInterfacePane(this, worldCanvas, worldState.worldHexField, factor)

  menu.prefWidth <== this.width - this.stateLabel.width
  menu.layoutX <== this.stateLabel.width
  stateLabel.prefHeight <== menu.height
  interfacePane.layoutY <== menu.height
  interfacePane.prefWidth <== this.width
  interfacePane.prefHeight <== this.height - menu.height

  this.children = List(stateLabel, menu, interfacePane)

  worldCanvas.onMouseClicked = (event: MouseEvent) => {
    info(s"clicked on (${event.x}, ${event.y})")
    val rect = worldCanvas.viewRect
    val provinceOpt = mapView.provinceByPixel(event.x + rect.minX toInt, event.y + rect.minY toInt)
    provinceOpt.foreach { p =>
      info(s"selected province ${p.name}")
      val pane = new InterfacePane(new ProvinceDetailsPane(p, this), () => {
        interfacePane.removeRightTopPanel()
        interfacePane.removeFacePanel()
      })
      interfacePane.setRightTopPanel(pane)
    }
  }

  def showPopulationPane(province: Province) {
    val pane = new PopulationViewPane(province)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showEnterprisesPane(province: Province): Unit = {
    val pane = new EnterprisesViewPane(province, sceneManager.stage, worldState)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showArmyMovement(province: Province): Unit = {
    val pane = new ArmyPane(province, provinceViewsMap(province), worldState, sceneManager.stage)
    interfacePane.setFullPanel(new InterfacePane(pane, () => {
      if (pane.soldiersWhereChanged) {
        worldCanvas.redraw()
      }
      hideFullPane()
    }))
  }

  def showMarket(province: Province): Unit = {
    val pane = new MarketViewPane(province, sceneManager.stage)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showBudgetPane(): Unit = {
    val pane = new BudgetPane(worldState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def showParliamentPane(): Unit = {
    val pane = new ParliamentPane(sceneManager, worldState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def hideFacePane(): Unit = {
    interfacePane.removeFacePanel()
  }

  def hideFullPane(): Unit = {
    interfacePane.removeFullPanel()
  }

  def showMinimap(): Unit = {
    interfacePane.showMinimap()
  }

  def hideMinimap(): Unit = {
    interfacePane.hideMinimap()
  }

  def nextTurn(): Unit = {
    worldState.nextTurn()
  }

  this.addEventFilter(javafx.scene.input.KeyEvent.KEY_PRESSED, new EventHandler[JKeyEvent] {
    override def handle(t: JKeyEvent): Unit = {
      if (Set(JKeyCode.UP, JKeyCode.DOWN, JKeyCode.LEFT, JKeyCode.RIGHT).contains(t.getCode)) {
        keyPressed(t)
        t.consume()
      }
    }
  })

  private def keepHValueInBounds(hv: Double): Double = {
    if (hv < 0) 0
    else if (hv > 1) 1
    else hv
  }

  private def keepWValueInBounds(wv: Double): Double = {
    if (wv < 0) 0
    else if (wv > 1) 1
    else wv
  }

  private def keyPressed(event: KeyEvent): Unit = {
    info(s"Pressed ${event.code}")
    event.code match {
      case KeyCode.Up =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelHeight
        val v = keepHValueInBounds(worldCanvas.vvalue.value - d)
        worldCanvas.vvalue.value = v
      case KeyCode.Down =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelHeight
        val v = keepHValueInBounds(worldCanvas.vvalue.value + d)
        worldCanvas.vvalue.value = v
      case KeyCode.Left =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelWidth
        val v = keepWValueInBounds(worldCanvas.hvalue.value - d)
        worldCanvas.hvalue.value = v
      case KeyCode.Right =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelWidth
        val v = keepWValueInBounds(worldCanvas.hvalue.value + d)
        worldCanvas.hvalue.value = v
      case _ => // do nothing
    }
  }

  class PlayerStateData(state: State) extends MigPane("center") with WorldInterfaceJavaNode {
    val rect = Rectangle(Components.largeFontSize, Components.largeFontSize)
    rect.fill = state.color
    rect.stroke = Color.Black
    val text = BigText(state.name)
    add(rect)
    add(text)
  }
}


