package mr.merc.ui.world

import mr.merc.local.Localization
import mr.merc.map.hex.TerrainHexField
import mr.merc.ui.common.CanvasLayers
import mr.merc.ui.minimap.Minimap
import scalafx.scene.layout.Pane
import scalafx.Includes._


class WorldInterfacePane(frame: WorldFrame, val worldCanvas: CanvasLayers, terrainField: TerrainHexField, factor:Double) extends Pane {
  private val minimapChild = new Minimap(terrainField, worldCanvas, factor)
  private val minimap: Pane = new MinimapParent(minimapChild)
  private val endTurnButton = BigButton(Localization("next.turn"))
  endTurnButton.onAction = { e =>
    frame.nextTurn()
  }

  children = List(worldCanvas, minimap, endTurnButton)

  worldCanvas.prefWidth <== this.width
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
  }

  private var rightTopPanel: Option[Pane] = None

  private var facePanel: Option[Pane] = None

  private var fullPanel: Option[Pane] = None

  def setRightTopPanel(pane: Pane): Unit = {
    removeRightTopPanel()
    rightTopPanel = Some(pane)
    children.add(pane)
    pane.layoutX <== this.width - this.width / 5
    pane.layoutY = 0
    pane.prefWidth <== this.width / 5
    pane.prefHeight <== this.height - this.width / 5
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

  def refreshMinimap(): Unit = {
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
