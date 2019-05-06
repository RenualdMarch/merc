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
import org.tbee.javafx.scene.layout.MigPane
import scalafx.Includes._

import scala.collection.mutable

class ArmyMovementPane(province: Province, provinceView: ProvinceView) extends MigPane with WorldInterfaceJavaNode {
  private val dragContext = mutable.Set[Warrior]()
  private val variants = province :: province.neighbours

}

class ArmyMovementPaneController(province: Province, provinceView: ProvinceView) {
  val soldiers: Map[Option[Province], ObservableBuffer[Warrior]] = (None :: province.neighbours.map(Some.apply)).map(p => p -> new ObservableBuffer[Warrior]()).toMap

  def refreshSoldiers(): Unit = {
    soldiers.foreach { case (key, buffer) =>
      val list = province.regionWarriors.warriorDestinations.getOrElse(key, Nil)
      buffer.clear()
      buffer ++= list
    }
  }
}

class WarriorsListTable(province: Province, soldiers: ObservableBuffer[Warrior], dragContext: mutable.Set[Warrior]) extends MigPane with WorldInterfaceWhiteJavaNode {
  val table = new TableView[Warrior]()

  def warriorToImageView(warrior: Warrior): ImageView = {
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
  add(table, "wrap")
  add(MediumText(province.name).delegate)
}
