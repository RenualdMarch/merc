package mr.merc.world.view

import mr.merc.unit.view.AbstractSoldierView
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.world.character.Character

class CharacterView(character: Character) extends AbstractSoldierView(SoldierTypeViewInfo(character.soldierType.name)) {

}