package mr.merc.ui.world

import scalafx.Includes._
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
import mr.merc.ui.world.selection.CharacterSelectedPanel
import mr.merc.ui.world.selection.CitySelectedPanel
import scalafx.scene.input.MouseEvent
import scalafx.scene.input.MouseButton
import scalafx.animation.Timeline
import scalafx.animation.KeyFrame
import scalafx.event.ActionEvent
import mr.merc.log.Logging
import scalafx.animation.Animation

class WorldFrame(sceneManager: SceneManager) extends BorderPane with Logging {
  val pulse = 20 ms

  val controller = new WorldController()

  val worldView = controller.worldView
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

  canvasLayers.handleEvent(MouseEvent.Any) {
    me: MouseEvent =>
      {
        me.eventType match {
          case MouseEvent.MousePressed => {
            me.button match {
              case MouseButton.PRIMARY => controller.onMouseLeftClick(me.x.toInt, me.y.toInt, canvasLayers.viewRect)
              case MouseButton.SECONDARY => controller.onMouseRightClick(me.x.toInt, me.y.toInt, canvasLayers.viewRect)
              case _ => // do nothing
            }
          }
          case MouseEvent.MouseMoved => controller.onMouseMove(me.x.toInt, me.y.toInt, canvasLayers.viewRect)
          case _ => // do nothing
        }
      }
  }

  def onSelectionChange {
    val newValue = controller.selected.value
    newValue match {
      case None => rightPanel.children.clear()
      case Some(c) => c match {
        case Left(l) => rightPanel.children.setAll(new CharacterSelectedPanel(l))
        case Right(r) => rightPanel.children.setAll(new CitySelectedPanel(r))
      }
    }
  }

  controller.selected.onChange(onSelectionChange)

  val timeline = Timeline(KeyFrame(pulse, onFinished = { ev: ActionEvent => gameLoop() }))
  timeline.cycleCount = Animation.INDEFINITE
  timeline.play()

  var lastUpdateTime = System.currentTimeMillis()
  def gameLoop() {
    val currentTime = System.currentTimeMillis
    val timePassed = currentTime - lastUpdateTime
    lastUpdateTime = currentTime
    //debug(s"in game loop $timePassed ms passed since previous call")
    controller.update(timePassed.toInt)
    canvasLayers.updateCanvas()
  }

}