package mr.merc.ui.world.selection

import mr.merc.world.character.Character
import scalafx.scene.canvas.Canvas
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.unit.view.StandState
import scalafx.scene.paint.Color
import scalafx.scene.layout.VBox
import scalafx.geometry.Pos._
import scalafx.scene.Node
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text
import mr.merc.local.Localization
import scalafx.geometry.Rectangle2D

class CharacterSelectedPanel(val character: Character) extends VBox {
  val side = 100

  val viewInfo = SoldierTypeViewInfo(character.soldierType.name, character.color)
  val image = viewInfo.images(StandState).head
  val characterCanvas = CanvasFactory(new Rectangle2D(0, 0, side, side), image)

  prefWidth = 400
  style = "-fx-background-color: cyan"
  spacing = 20
  alignment = TOP_CENTER
  content = List[Node](characterCanvas, new GridPane {
    vgap = 20
    hgap = 10
    alignment = TOP_LEFT
    style = "-fx-padding: 0 0 0 100;"
    add(new Text {
      text = Localization("character.name")
    }, 0, 0)
    add(new Text {
      text = character.name
    }, 1, 0)
    add(new Text {
      text = Localization("character.position")
    }, 0, 1)
    add(new Text {
      text = character.characterType.name
    }, 1, 1)
    add(new Text {
      text = Localization("character.armySize")
    }, 0, 2)
    add(new Text {
      text = "0"
    }, 1, 2)
  })

}

