package mr.merc.unit.view

import mr.merc.view.SpriteState
import mr.merc.view.Sprite
import mr.merc.image.MImage

object SoldierView {
  
}

class SoldierView private (images:Map[SoldierViewState, List[MImage]]) extends Sprite[SoldierViewState](images, StandState) {

}


