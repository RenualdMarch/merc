package mr.merc.ui.world

import mr.merc.army.Warrior
import mr.merc.local.Localization
import mr.merc.map.hex.view.ProvinceView
import mr.merc.politics.Province
import mr.merc.unit.view.StandState
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.image.ImageView
import scalafx.scene.layout.Pane
import mr.merc.util.FxPropertyUtils._
import scalafx.beans.property.ObjectProperty
import EconomicLocalization._

import scala.collection.mutable

class ArmyMovementPane(province:Province, provinceView:ProvinceView) extends Pane {

}

class WarriorsListTable(soldiers:ObservableBuffer[Warrior], dragContext:mutable.Set[Warrior]) {
  val table = new TableView[Warrior]()

  def warriorToImageView(warrior: Warrior):ImageView = {
    val image = warrior.soldierView(1d, false).images(StandState).head.image
    new ImageView(image)
  }

  val soldierImageColumn = new TableColumn[Warrior, ImageView] {
    text = Localization("army.warrior")
    cellValueFactory = p => ObjectProperty(warriorToImageView(p.value))
  }

  val soldierTypeColumn = new TableColumn[Warrior, String] {
    text = Localization("army.warriorType")
    cellValueFactory = p => ObjectProperty(localizeWarriorType(p.value))
    editable = false
  }

  val soldierHP = new TableColumn[Warrior, String] {
    text = Localization("army.hp")
    cellValueFactory = p => ObjectProperty(IntFormatter().format(p.value.hpPercentage * 100) + "%")
    editable = false
  }

  table.columns ++= List(soldierImageColumn, soldierTypeColumn, soldierHP)
}
