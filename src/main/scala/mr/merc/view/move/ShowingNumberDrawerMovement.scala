package mr.merc.view.move

import scalafx.scene.paint.Color
import mr.merc.view.Drawable
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight
import scalafx.geometry.Rectangle2D

object ShowingNumberDrawerMovement {
  val TextSize = 40

  private val speed = 120
  private val height = 60
  private val fadingStart = 0.7

  def damage(x: Int, y: Int, number: Int): ShowingNumberDrawerMovement = {
    new ShowingNumberDrawerMovement((x, y), speed, height, fadingStart, number.toString, Color.Red)
  }

  def drain(x: Int, y: Int, number: Int): ShowingNumberDrawerMovement = {
    new ShowingNumberDrawerMovement((x, y), speed, height, fadingStart, number.toString, Color.Green)
  }
}

class ShowingNumberDrawerMovement(target: (Int, Int), speed: Int, height: Int, fadingStart: Double, text: String, color: Color) extends Drawable with Movement {

  require(fadingStart >= 0 && fadingStart <= 1, "Fading start is out of bounds!")
  private val textLength = 54

  private val movement = new LinearMovement(target._1, target._2, target._1, target._2 - height, speed)

  var dirtyRect: Option[Rectangle2D] = None

  override def viewRect: Rectangle2D = new Rectangle2D(movement.x + textLength / 2, movement.y - ShowingNumberDrawerMovement.TextSize * 1.5, textLength, ShowingNumberDrawerMovement.TextSize * 3)

  override def start() {
    super.start()
    movement.start()
  }

  override def update(time: Int) {
    dirtyRect = Some(viewRect)
    super.update(time)
    movement.update(time)
  }

  def isOver = movement.coveredPart == 1

  def drawItself(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    if (movement.coveredPart > fadingStart) {
      gc.save()
      val fadingPart = 1 - fadingStart
      val fadingAlready = movement.coveredPart - fadingStart
      val alpha = 1 - fadingAlready / fadingPart
      gc.globalAlpha = alpha
      drawText(gc, xOffset, yOffset)
      gc.restore
    } else {
      drawText(gc, xOffset, yOffset)
    }
  }

  def drawText(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    gc.save()
    gc.font = Font.font(Font.default.getFamily, FontWeight.Bold, ShowingNumberDrawerMovement.TextSize)
    gc.fill = color
    gc.fillText(text, movement.x + textLength / 2 + xOffset, movement.y + yOffset, textLength)
    gc.restore()
  }
}