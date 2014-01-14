package mr.merc.view.move

import scalafx.scene.paint.Color
import mr.merc.view.Drawable
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.text.Font
import scalafx.scene.text.FontWeight

object ShowingNumberDrawerMovement {
  private val speed = 125
  private val height = 60
  private val fadingStart = 0.7

  def damage(x: Int, y: Int, number: Int): ShowingNumberDrawerMovement = {
    new ShowingNumberDrawerMovement((x, y), speed, height, fadingStart, number.toString, Color.RED)
  }

  def drain(x: Int, y: Int, number: Int): ShowingNumberDrawerMovement = {
    new ShowingNumberDrawerMovement((x, y), speed, height, fadingStart, number.toString, Color.GREEN)
  }
}

class ShowingNumberDrawerMovement(target: (Int, Int), speed: Int, height: Int, fadingStart: Double, text: String, color: Color) extends Drawable with Movement {

  require(fadingStart >= 0 && fadingStart <= 1, "Fading start is out of bounds!")
  private val textLength = 54

  private val movement = new LinearMovement(target._1, target._2, target._1, target._2 - height, speed)

  override def start() {
    super.start()
    movement.start()
  }

  override def update(time: Int) {
    super.update(time)
    movement.update(time)
  }

  def isOver = movement.coveredPart == 1

  def drawItself(gc: GraphicsContext) {
    if (movement.coveredPart > fadingStart) {
      gc.save()
      val fadingPart = 1 - fadingStart
      val fadingAlready = movement.coveredPart - fadingStart
      val alpha = 1 - fadingAlready / fadingPart
      gc.globalAlpha = alpha
      drawText(gc)
      gc.restore
    } else {
      drawText(gc)
    }
  }

  def drawText(gc: GraphicsContext) {
    gc.save()
    gc.font = Font.font(Font.default.getFamily, FontWeight.BOLD, 20)
    gc.fill = color
    gc.fillText(text, movement.x + textLength / 2, movement.y, textLength)
    gc.restore()
  }

  override def dirtyHexes = Nil // Seems like attack movement must take care of it alredy
}