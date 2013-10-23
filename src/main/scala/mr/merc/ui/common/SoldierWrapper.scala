package mr.merc.ui.common

import mr.merc.unit.Soldier
import scalafx.beans.property.IntegerProperty
import scalafx.beans.property.StringProperty
import scalafx.beans.property.ObjectProperty
import scalafx.scene.image.Image
import mr.merc.unit.SoldierType
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.unit.view.SoldierView
import mr.merc.unit.view.StandState
import mr.merc.image.MImage

class SoldierWrapper(private var _soldier:Soldier) {
	val hp = StringProperty("")
	val name = StringProperty("")
	val exp = StringProperty("")
	val expToNextLevel = StringProperty("")
	val movePoints = StringProperty("")
	val movePointsTotal = StringProperty("")
	val image = ObjectProperty(standImage)
	val level = StringProperty("")
  
    refreshProperties()
  
    def soldier = _soldier
	def soldier_=(newSoldier:Soldier) {
	  _soldier = newSoldier
	  refreshProperties()
	}
	
	def refreshProperties() {
	  hp.value = _soldier.hp.toString
	  name.value = _soldier.name
	  exp.value = _soldier.currentExp.toString
	  expToNextLevel.value = _soldier.soldierType.exp.toString
	  movePoints.value = _soldier.movePointsRemain.toString
	  movePointsTotal.value = _soldier.soldierType.movement.toString
	  image.value = standImage
	  level.value = _soldier.soldierType.level.toString
	}
	
	private def standImage:MImage = {
	  val view = new SoldierView(_soldier)
	  view.state = StandState
	  view.images(StandState)(0)
	}
	
}