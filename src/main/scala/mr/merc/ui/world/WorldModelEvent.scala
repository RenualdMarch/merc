package mr.merc.ui.world

import mr.merc.map.world.Province

sealed trait WorldModelEvent {

}

case class MoveCharacter(character: Character, province: Province) extends WorldModelEvent
case class MoveCharacterToBorders(character: Character) extends WorldModelEvent