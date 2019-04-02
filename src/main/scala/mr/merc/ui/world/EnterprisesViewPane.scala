package mr.merc.ui.world

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
import scalafx.scene.Scene
import scalafx.scene.chart.{AreaChart, LineChart, NumberAxis, XYChart}
import scalafx.stage.{Modality, Stage}

import scala.collection.JavaConverters._

class EnterprisesViewPane(province: Province, stage: Stage) extends PaneWithTwoEqualHorizontalChildren {

  val enterpriseTablePane = new EnterprisesTablePane(province.enterprises)
  private def f(e:Enterprise) = new EnterpriseDetailsPane(e, province, e.dayRecords.last.turn, stage)
  val enterpriseDetailsPane: Pane = new PropertyDependentPane(enterpriseTablePane.selectedRow, f)

  setTwoChildren(enterpriseTablePane, enterpriseDetailsPane)
}


class EnterprisesTablePane(enterprises: Seq[Enterprise]) extends MigPane with WorldInterfaceJavaNode {
  private val enterprisesTable = new TableView[Enterprise]()
  enterprisesTable.style = Components.mediumFontStyle

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
    cellValueFactory = e => StringProperty(e.value.dayRecords.lastOption.map { p =>
      s"${DoubleFormatter().format(p.produced)}/${DoubleFormatter().format(p.itemsSold)}"
    }.getOrElse(""))
    editable = false
    prefWidth <== enterprisesTable.width * 0.20
  }

  private val workersColumn = new TableColumn[Enterprise, String] {
    text = Localization("workers")
    cellValueFactory = e => StringProperty(e.value.dayRecords.lastOption.map { p =>
      IntFormatter().format(p.peopleResources.values.sum)
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

class SupplyAndDemandPane(enterprise: Enterprise, turn: Int) extends MigPane {
  enterprise match {
    case e: Factory[_] =>
      val record = e.dayRecords.find(_.turn == turn).get

      val supplyTitle = BigText(Localization("supply"))
      val demandTitle = BigText(Localization("demand"))
      val supplyTable = SupplyDemandTables.buildEnterpriseSupplyTable(record)
      val demandTable = SupplyDemandTables.buildFactoryDemandTable(record)

      add(supplyTitle, "span, center, wrap")
      add(supplyTable, "span, grow, push, wrap")
      add(demandTitle, "span, center, wrap")
      add(demandTable, "span, grow, push, wrap")
    case e: ResourceGathering[_] =>
      val record = e.dayRecords.find(_.turn == turn).get

      val supplyTitle = BigText(Localization("supply"))
      val supplyTable = SupplyDemandTables.buildEnterpriseSupplyTable(record)

      add(supplyTitle, "span, center, wrap")
      add(supplyTable, "grow, push, span, wrap")
  }
}

class EnterpriseDetailsPane(enterprise: Enterprise, province: Province, turn:Int, stage: Stage) extends MigPane {

  val accordion = new Accordion()

  add(BigText(localizeEnterprise(enterprise, province)), "center, wrap")
  add(accordion, "grow,push,wrap")

  val mainPane: Pane = enterprise match {
    case e: Factory[_] => new FactoryPane(e, province)
    case e: ResourceGathering[_] => new ResourceGatheringPane(e, province)
  }

  val supplyAndDemand: Pane = new SupplyAndDemandPane(enterprise, turn)

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

  val historical = new TitledPane() {
    text = Localization("history")
    style = s"-fx-font-size: ${Components.largeFontSize}"
    content = new HistoricalEnterpriseRecords(enterprise, province, stage)
  }


  accordion.panes = List(mainTitle, supplyAndDemandTitle, historical)
  accordion.expandedPane = mainTitle
}

abstract class EnterprisePane(e: Enterprise, province: Province) extends MigPane() with WorldInterfaceJavaNode {


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
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.produced)).getOrElse("")
    }.delegate, "wrap")
  }

  def itemsSold() {
    add(MediumText(Localization("enterprise.itemsSold")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.itemsSold)).getOrElse("")
    }.delegate, "wrap")
  }

  def inStorage() {
    add(MediumText(Localization("enterprise.inStorage")))
    add(new MediumText {
      text = Localization(IntFormatter().format(e.unsoldProducts))
    }.delegate, "wrap")
  }

  def earnings() {
    add(MediumText(Localization("enterprise.earnings")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.earnings)).getOrElse("")
    }.delegate, "wrap")
  }

  def corporateTax() {
    add(MediumText(Localization("enterprise.corporateTax")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.corporateTax)).getOrElse("")
    }.delegate, "wrap")
  }

  def salary() {
    add(MediumText(Localization("enterprise.salary")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.moneyOnWorkforceSalary)).getOrElse("")
    }.delegate, "wrap")
  }

  def ownersProfit() {
    add(MediumText(Localization("enterprise.ownersProfit")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.moneyOnOwnersPayment)).getOrElse("")
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

  def spentOfResources() {
    add(MediumText(Localization("enterprise.spentOnResources")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.moneySpentOnResources)).getOrElse("")
    }.delegate, "wrap")
  }

  def budget() {
    add(MediumText(Localization("enterprise.budget")))
    add(new MediumText {
      text = IntFormatter().format(e.factoryStorage.money)
    }.delegate, "wrap")
  }

  def moneyToBudget() {
    add(MediumText(Localization("enterprise.moneyToBudget")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.moneyToFactoryBudget)).getOrElse("")
    }.delegate, "wrap")
  }

  def factoryProfit() {
    add(MediumText(Localization("enterprise.factoryProfit")))
    add(new MediumText {
      text = e.dayRecords.lastOption.map(d => IntFormatter().format(d.factoryBuySellProfit)).getOrElse("")
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

class HistoricalEnterpriseRecords(e: Enterprise, p:Province, stage: Stage) extends MigPane {

  def buildProductionChart(): Option[LineChart[Number, Number]] = {
    val size = e.dayRecords.size

    e.dayRecords.headOption.map { first =>
      val xAxis = new NumberAxis(first.turn, first.turn + size, 1)
      val yAxis = new NumberAxis()
      xAxis.label = Localization("weeks")
      yAxis.label = Localization("units")

      val chart = new LineChart[Number, Number](xAxis, yAxis)
      chart.title = Localization("production")
      chart.style = Components.mediumFontStyle

      val produced = new XYChart.Series[Number, Number]()
      produced.name = Localization("produced")
      e.dayRecords.foreach { h =>
        produced.getData.add(XYChart.Data[Number, Number](h.turn, h.produced))
      }

      val sold = new XYChart.Series[Number, Number]()
      sold.name = Localization("sold")
      e.dayRecords.foreach { h =>
        sold.getData.add(XYChart.Data[Number, Number](h.turn, h.itemsSold))
      }

      chart.legendVisible = true
      chart.getData.addAll(produced, sold)

      chart.lookupAll(".chart-line-symbol").asScala.foreach { s =>
        val dataNumber = s.getStyleClass.find(_.startsWith("data")).get.replace("data", "").toInt + first.turn
        s.onMouseClicked = _ => showProductionDialog(e, dataNumber)
      }

      chart
    }
  }

  def showProductionDialog(enterprise: Enterprise, turn: Int): Unit = {
    val pane: Pane = new SupplyAndDemandPane(enterprise, turn)

    pane.prefWidth <== stage.width * 0.5
    pane.prefHeight <== stage.height * 0.8

    val dialog = new Stage {
      title = Localization("market.day.info")
      scene = new Scene {
        content = pane
      }
    }

    dialog.initModality(Modality.WindowModal)
    dialog.initOwner(stage)
    dialog.centerOnScreen()
    dialog.show()
  }

  buildProductionChart().foreach { chart =>
    add(chart, "push, grow")
  }
}