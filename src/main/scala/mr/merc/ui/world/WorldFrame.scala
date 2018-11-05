package mr.merc.ui.world

import mr.merc.economics.WorldGenerator
import mr.merc.log.Logging
import mr.merc.map.hex.view.ProvinceView
import mr.merc.map.hex.view.TerrainHexFieldView.WorldMapViewMode
import mr.merc.map.view.MapView
import mr.merc.ui.common.{CanvasLayers, SceneManager}
import mr.merc.ui.minimap.Minimap
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.Pane
import scalafx.scene.input.MouseEvent
import scalafx.Includes._

class WorldFrame(sceneManager: SceneManager) extends Pane with Logging {
  val factor = 1d

  val (states, terrainField) = WorldGenerator.generateWorld()
  val mapView = new MapView(terrainField, factor, mode = WorldMapViewMode)
  val provinceViews = states.flatMap { case (_, pList) =>
    pList.map { p =>
      new ProvinceView(p, terrainField, mapView.terrainView)
    }
  }
  provinceViews.foreach(_.refreshCity())
  mapView.terrainView.refreshTerrainDirt()
  private val worldCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight))

  val interfacePane = new WorldInterfacePane

  interfacePane.prefWidth <== this.width
  interfacePane.prefHeight <== this.height

  this.children = List(interfacePane)

  worldCanvas.prefWidth <== this.width
  worldCanvas.prefHeight <== this.height

  worldCanvas.onMouseClicked = (event:MouseEvent) => {
    info(s"clicked on (${event.screenX}, ${event.screenY}")
    val rect = worldCanvas.viewRect
    val provinceOpt = mapView.provinceByPixel(event.screenX + rect.minX toInt, event.screenY + rect.minY toInt)
    provinceOpt.foreach { p =>
      info(s"selected province ${p.name}")
      val pane = new InterfacePane(new ProvinceDetailsPane(p, this), () => interfacePane.removeRightTopPanel())
      interfacePane.setRightTopPanel(pane)
    }
  }

  class WorldInterfacePane extends Pane {
    private val minimap: Pane = new MinimapParent(new Minimap(terrainField, worldCanvas, factor))

    children = List(worldCanvas, minimap)

    layoutX = 0
    layoutY = 0

    minimap.layoutX <== this.width - this.width / 5
    minimap.layoutY <== this.height - this.width / 5
    minimap.prefWidth <== this.width / 5
    minimap.prefHeight <== this.width / 5

    private var rightTopPanel: Option[Pane] = None
    private var facePanel: Option[Pane] = None

    def setRightTopPanel(pane: Pane): Unit = {
      removeRightTopPanel()
      rightTopPanel = Some(pane)
      children.add(pane)
      pane.layoutX <== this.width - this.width / 5
      pane.layoutY = 0
      pane.prefWidth <== this.width / 5
      pane.prefHeight <== this.height - this.width / 5
    }

    def removeRightTopPanel(): Unit = {
      rightTopPanel.foreach { p =>
        this.children.remove(p)
      }
      rightTopPanel = None
    }

    def removeFacePanel(): Unit ={
      facePanel.foreach { p =>
        this.children.remove(p)
      }
      facePanel = None
    }

    def setFacePanel(pane: Pane): Unit = {
      removeFacePanel()
      facePanel = Some(pane)
      children.add(pane)
      pane.layoutX <== 0
      pane.layoutY = 0
      pane.prefWidth <== this.width * 4 / 5
      pane.prefHeight <== this.height
    }

  }
}

