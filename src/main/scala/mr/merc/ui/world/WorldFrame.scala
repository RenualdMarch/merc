package mr.merc.ui.world

import mr.merc.ui.common.SceneManager
import scalafx.geometry.Pos._
import scalafx.scene.layout.BorderPane
import mr.merc.map.reader.WesnothMapReader
import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.view.SoldiersDrawer
import mr.merc.ui.common.CanvasLayers
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.VBox
import mr.merc.map.world.WorldMap

class WorldFrame(sceneManager: SceneManager) extends BorderPane {
  val worldMap = WorldMap.load("worldMap1")
  worldMap.initCharacters()
  worldMap.initHumanCharacter()
  val worldView = new WorldView(worldMap)

  //mapView.worldMapArrows = List((mapView.hex(11, 4), mapView.hex(6, 1)))
  val canvasLayers = new CanvasLayers(worldView.canvasLayers, new Rectangle2D(0, 0, worldView.pixelWidth, worldView.pixelHeight))

  private val rightPanel = new VBox() {
    prefWidth = 400
    style = "-fx-background-color: cyan"
    spacing = 20
    alignment = TOP_CENTER
  }

  center = canvasLayers
  right = rightPanel
  canvasLayers.prefWidth <== this.width - rightPanel.width
  canvasLayers.prefHeight <== this.height
}