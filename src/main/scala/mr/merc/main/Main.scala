package mr.merc.main

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Stop.sfxStop2jfx
import scalafx.scene.paint.Color
import scalafx.scene.paint.CycleMethod
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.Stop
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Screen
import mr.merc.map.hex.TerrainHexView
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHexFieldView
import scalafx.scene.image.Image
import mr.merc.map.terrain.Sand


object Main extends JFXApp {
  val map = new TerrainHexFieldView(new TerrainHexField(5, 5, mapInit))
  
  
  
  val screenRect = Screen.primary.visualBounds
  val canvas = new Canvas(screenRect.width, screenRect.height)

  // Draw background with gradient
  val rect = new Rectangle {
    height = screenRect.height
    width = screenRect.width
    fill = new LinearGradient(0, 0, 1, 1, true, CycleMethod.REFLECT, List(Stop(0, Color.RED), Stop(1, Color.YELLOW)))
  }

  val rootPane = new Group
  rootPane.children = List(rect, canvas)

  stage = new PrimaryStage {
    title = "Canvas Doodle Test"
    scene = new Scene(screenRect.width, screenRect.height) {
      root = rootPane
    }
  }
  
  val gc = canvas.graphicsContext2D

  reset(Color.BLUE)

  // Clear away portions as the user drags the mouse
  canvas.onMouseDragged = (e: MouseEvent) => {
    gc.clearRect(e.x - 2, e.y - 2, 5, 5)
  }

  // Fill the Canvas with a Blue rectnagle when the user double-clicks
  canvas.onMouseClicked = (e: MouseEvent) => {
    if (e.clickCount > 1) {
      reset(Color.BLUE);
    }
  }

  /**
   * Resets the canvas to its original look by filling in a rectangle covering
   * its entire width and height. Color.BLUE is used in this demo.
   *
   * @param color The color to fill
   */
  private def reset(color: Color) {
    gc.fill = color
    gc.fillRect(0, 0, canvas.width.get, canvas.height.get);
    map.hexes.foreach(h => {
      h.drawItself(gc)
    })
  }

  private def mapInit(x:Int, y:Int) = 
    if (x != 1 && y != 1) {
      new TerrainHex(x, y, Grass)
    } else {
      new TerrainHex(x, y, Sand)
    }

}