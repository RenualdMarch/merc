package mr.merc.world.view

import mr.merc.unit.view.AbstractSoldierView
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.world.character.Character
import scalafx.scene.canvas.GraphicsContext

class CharacterView(val character: Character) extends AbstractSoldierView(SoldierTypeViewInfo(character.soldierType.name, character.color)) {

  override def drawItself(gc: GraphicsContext, xOffset: Int, yOffset: Int) {
    drawOvalUnderSoldier(gc, xOffset: Int, yOffset: Int, character.color)
    super.drawItself(gc, xOffset, yOffset)
  }
}