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

class SoldierWrapper(private var _soldier:Option[Soldier]) {
	val hp = StringProperty("")
	val name = StringProperty("")
	val exp = StringProperty("")
	val expToNextLevel = StringProperty("")
	val movePoints = StringProperty("")
	val movePointsTotal = StringProperty("")
	val image = ObjectProperty(MImage.emptyImage)
	val level = StringProperty("")
  
    refreshProperties()
  
    def soldier = _soldier
	def soldier_=(newSoldier:Option[Soldier]) {
	  _soldier = newSoldier
	  refreshProperties()
	}
	
	def refreshProperties() {
	  _soldier match {
	    case Some(soldier) => {
	      hp.value = soldier.hp.toString
	      name.value = soldier.name
	      exp.value = soldier.currentExp.toString
	      expToNextLevel.value = soldier.soldierType.exp.toString
	      movePoints.value = soldier.movePointsRemain.toString
	      movePointsTotal.value = soldier.soldierType.movement.toString
	      image.value = standImage(soldier)
	      level.value = soldier.soldierType.level.toString
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
	    }
	  }
	  
	}
	
	private def standImage(soldier:Soldier):MImage = {
	  val view = new SoldierView(soldier)
	  view.state = StandState
	  view.images(StandState)(0)
	}
	
}