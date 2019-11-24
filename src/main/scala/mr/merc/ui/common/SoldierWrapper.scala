package mr.merc.ui.common

import mr.merc.unit.Soldier
import scalafx.beans.property.StringProperty
import scalafx.beans.property.ObjectProperty
import mr.merc.unit.view.SoldierView
import mr.merc.unit.view.StandState
import mr.merc.image.MImage
import mr.merc.local.Localization

class SoldierWrapper(private var _soldier: Option[Soldier], factor:Double) {
  val hp = StringProperty("")
  val name = StringProperty("")
  val exp = StringProperty("")
  val soldierType = StringProperty("")
  val expToNextLevel = StringProperty("")
  val movePoints = StringProperty("")
  val movePointsTotal = StringProperty("")
  val image = ObjectProperty(MImage.emptyImage)
  val level = StringProperty("")

  refreshProperties()

  def soldier = _soldier
  def soldier_=(newSoldier: Option[Soldier]) {
    _soldier = newSoldier
    refreshProperties()
  }

  def refreshProperties() {
    _soldier match {
      case Some(soldier) => {
        hp.value = soldier.hp.toString + "/" + soldier.soldierType.hp
        name.value = soldier.name
        exp.value = soldier.exp.toString
        expToNextLevel.value = soldier.soldierType.exp.toString
        movePoints.value = soldier.movePointsRemain.toString
        movePointsTotal.value = soldier.soldierType.movement.toString
        image.value = standImage(soldier).scaledImage(factor)
        level.value = soldier.soldierType.level.toString
        soldierType.value = Localization(soldier.soldierType.name)
      }
      case None => {
        hp.value = ""
        name.value = ""
        exp.value = ""
        expToNextLevel.value = ""
        movePoints.value = ""
        movePointsTotal.value = ""
        image.value = MImage.emptyImage
        level.value = ""
        soldierType.value = ""
      }
    }

  }

  private def standImage(soldier: Soldier): MImage = {
    val view = new SoldierView(soldier, 1.0)
    view.state = StandState
    view.images(StandState).head
  }

}