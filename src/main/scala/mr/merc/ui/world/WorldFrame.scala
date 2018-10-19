package mr.merc.ui.world

import mr.merc.economics.WorldGenerator
import mr.merc.map.hex.view.TerrainHexFieldView.WorldMapViewMode
import mr.merc.map.view.MapView
import mr.merc.ui.common.{CanvasLayers, SceneManager}
import mr.merc.ui.minimap.Minimap

import scalafx.geometry.{Insets, Rectangle2D}
import scalafx.scene.layout.{GridPane, Pane}

class WorldFrame(sceneManager: SceneManager) extends Pane {
  import scalafx.Includes._
  val factor = 1d

  val (states, terrainField) = WorldGenerator.generateWorld()
  val mapView = new MapView(terrainField, factor, mode = WorldMapViewMode)
  private val worldCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight), false)

  val interfacePane = new InterfacePane

  interfacePane.prefWidth <== this.width
  interfacePane.prefHeight <== this.height

  this.children = List(worldCanvas, interfacePane)

  worldCanvas.prefWidth <== this.width
  worldCanvas.prefHeight <== this.height

  class InterfacePane extends Pane {
    private val minimap = new Minimap(terrainField, worldCanvas, factor)
    children = List(minimap)

    layoutX = 0
    layoutY = 0

    minimap.layoutX <== this.width - this.width / 6
    minimap.layoutY <== this.height - this.width / 6
    minimap.prefWidth <== this.width / 6
    minimap.prefHeight <== this.width / 6
  }
}

