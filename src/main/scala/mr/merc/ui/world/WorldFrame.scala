package mr.merc.ui.world

import mr.merc.economics.WorldGenerator
import mr.merc.log.Logging
import mr.merc.map.hex.view.ProvinceView
import mr.merc.map.hex.view.TerrainHexFieldView.WorldMapViewMode
import mr.merc.map.view.MapView
import mr.merc.politics.Province
import mr.merc.ui.common.{CanvasLayers, SceneManager}
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.input.MouseEvent
import scalafx.Includes._

class WorldFrame(sceneManager: SceneManager) extends Pane with Logging {
  val factor = 1d

  val worldState = WorldGenerator.generateWorld()
  val mapView = new MapView(worldState.worldHexField, factor, mode = WorldMapViewMode)
  val menu = new WorldMenu(this)

  val provinceViews = worldState.states.flatMap { case (_, pList) =>
    pList.map { p =>
      new ProvinceView(p, worldState.worldHexField, mapView.terrainView)
    }
  }
  provinceViews.foreach(_.refreshCity())
  mapView.terrainView.refreshTerrainDirt()
  private val worldCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight))

  private val interfacePane = new WorldInterfacePane(this, worldCanvas, worldState.worldHexField, factor)

  menu.prefWidth <== this.width
  interfacePane.layoutY <== menu.height
  interfacePane.prefWidth <== this.width
  interfacePane.prefHeight <== this.height - menu.height

  this.children = List(menu, interfacePane)

  worldCanvas.onMouseClicked = (event:MouseEvent) => {
    info(s"clicked on (${event.x}, ${event.y}")
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
    val pane = new EnterprisesViewPane(province, sceneManager.stage)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showMarket(province: Province): Unit = {
    val pane = new MarketViewPane(province, sceneManager.stage)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showBudgetPane(): Unit = {
    val pane = new BudgetPane(worldState.playerState,
      worldState.regions.filter(_.owner == worldState.playerState))
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
}


