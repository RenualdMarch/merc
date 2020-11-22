package mr.merc.ui.world

import mr.merc.local.Localization
import mr.merc.map.hex.TerrainHexField
import mr.merc.ui.common.CanvasLayers
import mr.merc.ui.minimap.Minimap
import scalafx.scene.layout.Pane
import scalafx.Includes._


class WorldInterfacePane(frame: WorldFrame, val worldCanvas: CanvasLayers, terrainField: TerrainHexField, factor:Double, pixelWidth:Int, pixelHeight:Int) extends Pane {

  private val emptyPane = new Pane with WorldInterfaceNode

  private val minimapChild = new Minimap(terrainField, worldCanvas, factor, pixelWidth, pixelHeight, false)
  private val minimap: Pane = new MinimapParent(minimapChild)
  private val endTurnButton = BigButton(Localization("next.turn"))
  endTurnButton.onAction = { e =>
    frame.nextTurn()
  }

  children = List(worldCanvas, minimap, endTurnButton, emptyPane)

  worldCanvas.prefWidth <== this.width - minimap.width
  worldCanvas.prefHeight <== this.height

  layoutX = 0
  layoutY = 0

  rebindMinimapAndEndTurn(false)

  def rebindMinimapAndEndTurn(minimapCollapsed: Boolean): Unit = {
    if (minimapCollapsed) {
      minimap.layoutX <== this.width - this.width / 5
      minimap.layoutY <== this.height
      minimap.prefWidth <== this.width / 5
      minimap.prefHeight <== this.width / 5
    } else {
      minimap.layoutX <== this.width - this.width / 5
      minimap.layoutY <== this.height - this.width / 5
      minimap.prefWidth <== this.width / 5
      minimap.prefHeight <== this.width / 5
    }

    endTurnButton.layoutX <== minimap.layoutX
    endTurnButton.layoutY <== minimap.layoutY - 50
    endTurnButton.prefWidth <== minimap.width
    endTurnButton.prefHeight = 50

    emptyPane.layoutX <== this.width - this.width / 5
    emptyPane.layoutY = 0
    emptyPane.prefWidth <== this.width / 5
    emptyPane.prefHeight <== this.height - this.width / 5 - endTurnButton.prefHeight
  }

  private var rightTopPanel: Option[Pane] = None

  private var facePanel: Option[Pane] = None

  private var fullPanel: Option[Pane] = None

  def setRightTopPanel(pane: Pane, remove: Boolean = true): Unit = {
    removeRightTopPanel()

    rightTopPanel = Some(pane)
    children.add(pane)
    pane.layoutX <== this.width - this.width / 5
    pane.layoutY = 0
    pane.prefWidth <== this.width / 5
    pane.prefHeight <== this.height - this.width / 5 - endTurnButton.prefHeight
    pane.requestFocus()
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

  def removeFullPanel(): Unit = {
    fullPanel.foreach { p =>
      this.children.remove(p)
    }
    fullPanel = None
  }


  def showMinimap(): Unit = {
    rebindMinimapAndEndTurn(false)
  }

  def hideMinimap(): Unit = {
    rebindMinimapAndEndTurn(true)
  }

  def refreshMinimap(field:TerrainHexField): Unit = {
    minimapChild.terrainHexField = field
    minimapChild.refreshMapCanvas()
  }

  def setFacePanel(pane: Pane): Unit = {
    removeFacePanel()
    facePanel = Some(pane)
    children.add(pane)
    pane.layoutX <== 0
    pane.layoutY = 0
    pane.prefWidth <== this.width * 4 / 5
    pane.prefHeight <== this.height
    pane.requestFocus()
  }

  def setFullPanel(pane: Pane): Unit = {
    removeFullPanel()
    fullPanel = Some(pane)
    children.add(pane)
    pane.layoutX = 0
    pane.layoutY = 0
    pane.prefWidth <== this.width
    pane.prefHeight <== this.height
    pane.requestFocus()
  }
}
