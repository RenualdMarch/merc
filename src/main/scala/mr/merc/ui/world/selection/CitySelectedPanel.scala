package mr.merc.ui.world.selection

import scalafx.geometry.Pos._
import mr.merc.map.world.Province
import mr.merc.image.MImage
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.VBox
import scalafx.scene.Node
import scalafx.scene.layout.GridPane
import scalafx.scene.text.Text
import mr.merc.local.Localization

class CitySelectedPanel(province: Province) extends VBox {
  val side = 100
  val image = MImage(province.settlement.picturePath)
  val canvas = CanvasFactory(new Rectangle2D(0, 0, side, side), image)
  val settlement = province.settlement

  // TODO move this to parent class
  prefWidth = 400
  style = "-fx-background-color: cyan"
  spacing = 20
  alignment = TOP_CENTER
  content = List[Node](canvas, new GridPane {
    vgap = 20
    hgap = 10
    alignment = TOP_LEFT
    style = "-fx-padding: 0 0 0 100;"
    add(new Text {
      text = Localization("city.name")
    }, 0, 0)
    add(new Text {
      text = Localization(settlement.nameKey)
    }, 1, 0)
    add(new Text {
      text = Localization("city.culture")
    }, 0, 1)
    add(new Text {
      text = Localization(settlement.cultureName)
    }, 1, 1)
    add(new Text {
      text = Localization("city.population")
    }, 0, 2)
    add(new Text {
      text = settlement.population.toString
    }, 1, 2)
  })
}