package mr.merc.ui.world

import java.text.DecimalFormat

import javafx.scene.control.SelectionMode
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.politics.Province
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control.{Accordion, TableColumn, TableView, TitledPane}
import scalafx.scene.layout.{BorderPane, Pane, TilePane}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import EconomicLocalization._

import scala.collection.JavaConverters._

class EnterprisesViewPane(province: Province) extends PaneWithTwoEqualHorizontalChildren {

  val enterpriseTablePane = new EnterprisesTablePane(province.enterprises)
  private def f(e:Enterprise) = new EnterpriseDetailsPane(e, province)
  val enterpriseDetailsPane: Pane = new PropertyDependentPane(enterpriseTablePane.selectedRow, f)

  setTwoChildren(enterpriseTablePane, enterpriseDetailsPane)
}


class EnterprisesTablePane(enterprises: Seq[Enterprise]) extends MigPane with WorldInterfaceJavaNode {
  private val enterprisesTable = new TableView[Enterprise]()
  enterprisesTable.style = s"-fx-font-size: ${Components.mediumFontSize}"

  private val productColumn = new TableColumn[Enterprise, String] {
    text = Localization("product")
    cellValueFactory = e => StringProperty(Localization(e.value.product.name))
    editable = false
    prefWidth <== enterprisesTable.width * 0.20
  }

  private val enterpriseTypeColumn = new TableColumn[Enterprise, String] {
    text = Localization("type")
    cellValueFactory = e => StringProperty(title(e.value))
    editable = false
    prefWidth <== enterprisesTable.width * 0.20
  }

  private val levelColumn = new TableColumn[Enterprise, String] {
    text = Localization("level")
    cellValueFactory = e => StringProperty(e.value match {
      case e: IndustrialFactory => e.level.toString
      case _ => ""
    })
    editable = false
    prefWidth <== enterprisesTable.width * 0.19
  }

  private val producedSoldColumn = new TableColumn[Enterprise, String] {
    text = Localization("producedAndSold")
    private val format = new DecimalFormat("#0.00")
    cellValueFactory = e => StringProperty(e.value.dayRecords.lastOption.map { p =>
      s"${format.format(p.produced)}/${format.format(p.itemsSold)}"
    }.getOrElse(""))
    editable = false
    prefWidth <== enterprisesTable.width * 0.20
  }

  private val workersColumn = new TableColumn[Enterprise, String] {
    text = Localization("workers")
    private val format = new DecimalFormat("#0")
    cellValueFactory = e => StringProperty(e.value.dayRecords.lastOption.map { p =>
      format.format(p.peopleResources.values.sum)
    }.getOrElse(""))
    editable = false
    prefWidth <== enterprisesTable.width * 0.20
  }

  private def sortQ(e: Enterprise): (Int, Int, String) = {
    e match {
      case f: IndustrialFactory => (5, -f.level, f.product.name)
      case f: Farm => (1, 0, f.product.name)
      case m: Mine => (2, 0, m.product.name)
      case c: Church => (3, 0, c.product.name)
      case mg: MagicGuildEnterprise => (4, 0, mg.product.name)
    }
  }

  private val buffer = new ObservableBuffer[Enterprise]()
  buffer.addAll(enterprises.sortBy(sortQ).reverse.asJava)
  enterprisesTable.items = buffer

  enterprisesTable.columns ++= List(enterpriseTypeColumn, productColumn, levelColumn, producedSoldColumn, workersColumn)
  enterprisesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  val selectedRow: ReadOnlyObjectProperty[Enterprise] = enterprisesTable.delegate.getSelectionModel.selectedItemProperty
  enterprisesTable.delegate.getSelectionModel.clearAndSelect(0)

  add(enterprisesTable, "growx,growy,pushx,pushy")
}

class EnterpriseDetailsPane(enterprise: Enterprise, province: Province) extends BorderPane {

  val accordion = new Accordion()

  center = accordion

  val mainPane: Pane = enterprise match {
    case e: Factory[_] => new FactoryPane(e, province)
    case e: ResourceGathering[_] => new ResourceGatheringPane(e, province)
  }

  val supplyAndDemand: Pane = enterprise match {
    case e: Factory[_] =>
      val pane = new MigPane()

      val supplyTitle = BigText(Localization("supply"))
      val demandTitle = BigText(Localization("demand"))
      val supplyTable = SupplyDemandTables.buildEnterpriseSupplyTable(e.dayRecords.last)
      val demandTable = SupplyDemandTables.buildFactoryDemandTable(e.dayRecords.last)

      pane.add(supplyTitle, "span, center, wrap")
      pane.add(supplyTable, "span, grow, push, wrap")
      pane.add(demandTitle, "span, center, wrap")
      pane.add(demandTable, "span, grow, push, wrap")

      pane
    case e: ResourceGathering[_] =>
      val pane = new MigPane()
      val supplyTitle = BigText(Localization("supply"))
      val supplyTable = SupplyDemandTables.buildEnterpriseSupplyTable(e.dayRecords.last)

      pane.add(supplyTitle, "span, center, wrap")
      pane.add(supplyTable, "grow, push, span, wrap")

      pane
  }

  val mainTitle = new TitledPane() {
    content = mainPane
    text = Localization("generalInfo")
    style = s"-fx-font-size: ${Components.largeFontSize}"
  }

  val supplyAndDemandTitle = new TitledPane() {
    text = Localization("production")
    style = s"-fx-font-size: ${Components.largeFontSize}"
    content = supplyAndDemand
  }


  accordion.panes = List(mainTitle, supplyAndDemandTitle)
  accordion.expandedPane = mainTitle
}

abstract class EnterprisePane(e: Enterprise, province: Province) extends MigPane() with WorldInterfaceJavaNode {
  private val asIntFormat = new DecimalFormat("#0")

  add(new BigText {
    text = localizeEnterprise(e, province)
  }.delegate, "span,center")

  add(MediumText(Localization("enterprise.type")))
  add(new MediumText {
    text = title(e)
  }.delegate, "wrap")

  add(MediumText(Localization("enterprise.product")))
  add(new MediumText {
    text = Localization(e.product.name)
  }.delegate, "wrap")

  def produced() {
    add(MediumText(Localization("enterprise.produced")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.produced)).getOrElse("")
    }.delegate, "wrap")
  }

  def itemsSold() {
    add(MediumText(Localization("enterprise.itemsSold")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.itemsSold)).getOrElse("")
    }.delegate, "wrap")
  }

  def inStorage() {
    add(MediumText(Localization("enterprise.inStorage")))
    add(new MediumText {
      text = Localization(asIntFormat.format(e.unsoldProducts))
    }.delegate, "wrap")
  }

  def earnings() {
    add(MediumText(Localization("enterprise.earnings")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.earnings)).getOrElse("")
    }.delegate, "wrap")
  }

  def corporateTax() {
    add(MediumText(Localization("enterprise.corporateTax")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.corporateTax)).getOrElse("")
    }.delegate, "wrap")
  }

  def salary() {
    add(MediumText(Localization("enterprise.salary")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.moneyOnWorkforceSalary)).getOrElse("")
    }.delegate, "wrap")
  }

  def ownersProfit() {
    add(MediumText(Localization("enterprise.ownersProfit")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.moneyOnOwnersPayment)).getOrElse("")
    }.delegate, "wrap")
  }

}

class ResourceGatheringPane(r: ResourceGathering[_], province: Province) extends EnterprisePane(r, province) {

  produced()
  inStorage()
  itemsSold()
  earnings()
  salary()
  ownersProfit()
  corporateTax()

}

class FactoryPane(e: Factory[_], province: Province) extends EnterprisePane(e, province) {
  private val asIntFormat = new DecimalFormat("#0")

  def spentOfResources() {
    add(MediumText(Localization("enterprise.spentOnResources")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.moneySpentOnResources)).getOrElse("")
    }.delegate, "wrap")
  }

  def budget() {
    add(MediumText(Localization("enterprise.budget")))
    add(new MediumText {
      text = asIntFormat.format(e.factoryStorage.money)
    }.delegate, "wrap")
  }

  def moneyToBudget() {
    add(MediumText(Localization("enterprise.moneyToBudget")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.moneyToFactoryBudget)).getOrElse("")
    }.delegate, "wrap")
  }

  def factoryProfit() {
    add(MediumText(Localization("enterprise.factoryProfit")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => asIntFormat.format(d.factoryBuySellProfit)).getOrElse("")
    }.delegate, "wrap")
  }

  budget()
  inStorage()
  produced()
  itemsSold()
  earnings()
  moneyToBudget()
  spentOfResources()
  salary()
  ownersProfit()
  factoryProfit()
  corporateTax()

}