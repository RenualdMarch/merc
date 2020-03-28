package mr.merc.ui.world

import javafx.scene.control.SelectionMode
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.politics.Province
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control._
import javafx.scene.control.TableCell
import scalafx.scene.layout.Pane
import scalafx.Includes._
import EconomicLocalization._
import mr.merc.economics.Products.IndustryProduct
import scalafx.scene.Scene
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.TabPane.TabClosingPolicy.Unavailable
import scalafx.stage.Stage
import mr.merc.ui.dialog.ModalDialog._
import mr.merc.util.FxPropertyUtils._
import mr.merc.economics.WorldStateEnterpriseActions.{StateBuildFactoryCommand, StateExpandFactoryCommand}
import scalafx.beans.binding.Bindings
import scalafx.collections.ObservableBuffer
import scalafx.util.StringConverter

import scala.collection.JavaConverters._

class EnterprisesViewPane(province: Province, stage: Stage, enterpriseActions: WorldStateEnterpriseActions) extends PaneWithTwoHorizontalChildren {

  private val controller = new EnterprisePaneController(province, enterpriseActions)

  private val enterprisesPane = new PaneWithTwoVerticalChildren(0.55)
  private val projectsPane = new RegionProjectsPane(controller.projects)
  private val enterpriseTablePane = new EnterprisesTablePane(province.enterprises, controller)
  private def f(either:Either[Enterprise, BusinessProject]) = {
    Option(either) match {
      case Some(Left(e)) =>
        projectsPane.projectsTable.delegate.getSelectionModel.clearSelection()
        new EnterpriseDetailsPane(e, province, e.dayRecords.last.turn, stage)
      case Some(Right(p)) =>
        enterpriseTablePane.enterprisesTable.delegate.getSelectionModel.clearSelection()
        new ProjectPane(p)
      case None => new MigPane() with WorldInterfaceJavaNode
    }
  }
  private val biProperty = bindTwoProperties(enterpriseTablePane.selectedRow, projectsPane.selectedRow)
  private val enterpriseDetailsPane: Pane = new PropertyDependentPane(biProperty, f)

  enterprisesPane.setTwoChildren(enterpriseTablePane, projectsPane)
  setTwoChildren(enterprisesPane, enterpriseDetailsPane)
}

class EnterprisePaneController(province: Province, worldState:WorldStateEnterpriseActions) {

  val enterprises:Vector[Enterprise] = province.enterprises

  val projects:ObservableBuffer[BusinessProject] = new ObservableBuffer[BusinessProject]()
  projects.addAll(province.projects.asJava)

  def expandDisabled(f:IndustrialFactory):Boolean = !worldState.stateCanExpandFactory(f) || f.region.owner != worldState.playerState

  def expandFactory(f:IndustrialFactory): Unit = {
    worldState.applyCommand(StateExpandFactoryCommand(worldState.playerState, f))
    projects.clear()
    projects.addAll(province.projects.asJava)
  }

  def buildFactory(product: IndustryProduct): Unit = {
    worldState.applyCommand(StateBuildFactoryCommand(province.owner, product, province))
    projects.clear()
    projects.addAll(province.projects.asJava)
    possibleFactories.clear()
    possibleFactories.addAll(possibleFactoriesToBuild.asJava)
  }

  val possibleFactories:ObservableBuffer[IndustryProduct] = new ObservableBuffer[IndustryProduct]
  possibleFactories.clear()
  possibleFactories.addAll(possibleFactoriesToBuild.asJava)

  private def possibleFactoriesToBuild:List[IndustryProduct] = {
    if (worldState.stateCanBuildFactory(province.owner) && worldState.playerState == province.owner) {
      Products.IndustryProducts.filterNot(province.presentFactoriesAndProjects.contains)
    } else Nil
  }
}

class EnterprisesTablePane(enterprises: Vector[Enterprise], controller:EnterprisePaneController) extends MigPane() with WorldInterfaceJavaNode {
  val enterprisesTable = new TableView[Enterprise]()
  enterprisesTable.style = Components.mediumFontStyle

  private val productColumn = new TableColumn[Enterprise, String] {
    text = Localization("product")
    cellValueFactory = e => StringProperty(localizeProduct(e.value.product))
    editable = false
    prefWidth <== enterprisesTable.width * 0.15
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
    prefWidth <== enterprisesTable.width * 0.09
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
    prefWidth <== enterprisesTable.width * 0.15
  }

  private val expandButtonColumn = new TableColumn[Enterprise, Button] {
    text = Localization("expand")
    cellFactory = p => new TableCell[Enterprise, Button] {

      def button:Button = getTableView.getItems.get(getIndex) match {
        case f:IndustrialFactory =>
          new MediumButton() {
            text = Localization("expandFactory", localizeProduct(f.product))
            disable = controller.expandDisabled(f)
            onAction = {_ =>
              controller.expandFactory(f)
            }
          }
        case _ => null
      }

      override def updateItem(t: Button, b: Boolean): Unit = {
        super.updateItem(t, b)
        if (b) {
          setGraphic(null)
        } else {
          setGraphic(button)
        }
      }
    }
    editable = false
    prefWidth <== enterprisesTable.width * 0.15
  }

  private def sortQ(e: Enterprise): (Int, Int, String) = {
    e match {
      case f: IndustrialFactory => (5, -f.level, f.product.name)
      case f: Farm => (1, 0, f.product.name)
      case m: Mine => (2, 0, m.product.name)
      case c: Church => (3, 0, c.product.name)
      case mg: MagicGuild => (4, 0, mg.product.name)
    }
  }

  private val enterprisesItems = new ObservableBuffer[Enterprise]()
  enterprisesItems.addAll(enterprises.sortBy(sortQ).reverse.asJava)
  enterprisesTable.items = enterprisesItems

  enterprisesTable.columns ++= List(enterpriseTypeColumn, productColumn, levelColumn, producedSoldColumn, workersColumn, expandButtonColumn)
  enterprisesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  val selectedRow: ReadOnlyObjectProperty[Enterprise] = enterprisesTable.delegate.getSelectionModel.selectedItemProperty
  enterprisesTable.delegate.getSelectionModel.clearAndSelect(0)

  add(enterprisesTable, "growx,growy,pushx,pushy,wrap")
  private val productSelection = new ComboBox[IndustryProduct] {
    style = Components.mediumFontStyle
    converter = new StringConverter[IndustryProduct] {
      override def fromString(string: String): IndustryProduct = null

      override def toString(t: IndustryProduct): String =
        Option(t).map(localizeProduct).getOrElse("")
    }
    items = controller.possibleFactories
    disable <== Bindings.createBooleanBinding(() => controller.possibleFactories.isEmpty, controller.possibleFactories)
  }

  private val bottomPane = new MigPane() {
    add(new MediumButton {
      text = Localization("buildFactory")
      disable <== Bindings.createBooleanBinding(() => productSelection.selectionModel.value.getSelectedItem == null, productSelection.selectionModel.value.selectedItemProperty())
      onAction = { _ =>
        val p = productSelection.selectionModel.value.getSelectedItem
        controller.buildFactory(p)
      }
    }.delegate, "")
    add(new MediumText {
      text = Localization("buildFactoryForProduct")
    }.delegate, "")
    add(productSelection.delegate, "wrap")
  }

  add(bottomPane, "center")

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

  private val tabPane = new TabPane() {
    style = Components.mediumFontStyle
    tabClosingPolicy = Unavailable
  }

  add(BigText(localizeEnterprise(enterprise, province)), "center, wrap")
  add(tabPane, "grow,push,wrap")

  private val mainTitle = new Tab() {
    content = enterprise match {
      case e: Factory[_] => new FactoryPane(e, province)
      case e: ResourceGathering[_] => new ResourceGatheringPane(e, province)
    }
    text = Localization("generalInfo")
    style = Components.largeFontStyle
  }

  private val supplyAndDemandTitle = new Tab() {
    text = Localization("production")
    style = Components.largeFontStyle
    content = new SupplyAndDemandPane(enterprise, turn)
  }

  private val historical = new Tab() {
    text = Localization("history")
    style = Components.largeFontStyle
    content = new HistoricalEnterpriseRecords(enterprise, province, stage)
  }

  tabPane.tabs = List(mainTitle, supplyAndDemandTitle, historical)

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
  //inStorage()
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

    dialog.showDialog(stage)
  }

  buildProductionChart().foreach { chart =>
    add(chart, "push, grow")
  }
}

class RegionProjectsPane(buffer:ObservableBuffer[BusinessProject]) extends MigPane with WorldInterfaceJavaNode {
  val projectsTable = new TableView[BusinessProject]()
  projectsTable.style = Components.mediumFontStyle

  private val projectColumn = new TableColumn[BusinessProject, String] {
    text = Localization("project.project")
    cellValueFactory = e => StringProperty(localizeProjectShort(e.value))
    editable = false
    prefWidth <== projectsTable.width * 0.45
  }

  private val moneyColumn = new TableColumn[BusinessProject, String] {
    text = Localization("project.remainingMoney")
    cellValueFactory = e => StringProperty(DoubleFormatter().format(e.value.remainingMoney))
    editable = false
    prefWidth <== projectsTable.width * 0.25
  }

  private val progressColumn = new TableColumn[BusinessProject, ProgressBar]() {
    text = Localization("project.progress")
    cellFactory = p => new TableCell[BusinessProject, ProgressBar] {
      override def updateItem(t: ProgressBar, b: Boolean): Unit = {
        super.updateItem(t, b)
        Option(t).foreach { t =>
          t.prefWidth <== this.widthProperty()
        }
        setGraphic(t)
      }
    }
    cellValueFactory = p => ObjectProperty[ProgressBar]{
      new ProgressBar() {
        progress = p.value.progress
      }
    }
    editable = false
    prefWidth <== projectsTable.width * 0.25
  }

  val selectedRow: ReadOnlyObjectProperty[BusinessProject] = projectsTable.delegate.getSelectionModel.selectedItemProperty

  projectsTable.columns ++= List(projectColumn, moneyColumn, progressColumn)
  projectsTable.items = buffer
  projectsTable.style = Components.mediumFontStyle

  add(projectsTable, "grow, push")
}

class ProjectPane(project: BusinessProject) extends MigPane with WorldInterfaceJavaNode {
  add(new BigText {
    text = localizeProjectShort(project)
  }.delegate, "center,span 2, wrap")

  add(MediumText(Localization("project")))
  add(new MediumText {
    text = localizeProject(project)
  }.delegate, "wrap")

  add(MediumText(Localization("project.remainingMoney")))
  add(new MediumText {
    text = DoubleFormatter().format(project.remainingMoney)
  }.delegate, "wrap")

  add(MediumText(Localization("project.remainingResources")))
  add(new MediumText {
    text = localizeProductsBucket(project.remainingProducts)
  }.delegate, "wrap")

  add(MediumText(Localization("project.alreadyBoughtProducts")))
  add(new MediumText {
    text = localizeProductsBucket(project.alreadyBoughtProducts)
  }.delegate, "wrap")
}