package mr.merc.ui.world

import java.text.DecimalFormat

import javafx.scene.control.SelectionMode
import mr.merc.economics.{Population, RegionPopulation}
import mr.merc.local.Localization
import mr.merc.politics._
import mr.merc.ui.world.PieChartBuilder.PiePart
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.Includes._
import scalafx.beans.property.{ReadOnlyObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control.{Accordion, TableColumn, TableView, TitledPane}

import scala.collection.JavaConverters._

class PopulationViewPane(province: Province) extends Pane {
  val popsTablePane = new PopsTablePane(province.regionPopulation)
  val popsTablePaneScala: Pane =  popsTablePane
  val popDetailsPane: Pane = new PopDetailsPane(popsTablePane.selectedRow, province)

  popsTablePaneScala.layoutX = 0
  popsTablePaneScala.layoutY = 0
  popsTablePaneScala.prefWidth <== this.width / 2
  popsTablePaneScala.prefHeight <== this.height

  popDetailsPane.layoutX <== this.width / 2
  popDetailsPane.layoutY = 0
  popDetailsPane.prefWidth <== this.width / 2
  popDetailsPane.prefHeight <== this.height

  this.children.addAll(popsTablePane, popDetailsPane)
}

class PopsTablePane(regionPopulation: RegionPopulation) extends MigPane with WorldInterfaceJavaNode {
  private val populationTable = new TableView[Population]()
  populationTable.style = s"-fx-font-size: ${Components.mediumFontSize}"

  private val raceColumn = new TableColumn[Population, String] {
    text = Localization("race")
    cellValueFactory = p => StringProperty(Localization(p.value.culture.race.name))
    editable = false
    prefWidth <== populationTable.width * 0.15
  }

  private val cultureColumn = new TableColumn[Population, String] {
    text = Localization("culture")
    cellValueFactory = p => StringProperty(Localization(p.value.culture.name))
    editable = false
    prefWidth <== populationTable.width * 0.15
  }

  private val populationFormatter = new DecimalFormat()
  populationFormatter.setGroupingSize(3)
  populationFormatter.setGroupingUsed(true)
  private val populationCountColumn = new TableColumn[Population, String] {
    text = Localization("population")
    cellValueFactory = p => StringProperty(populationFormatter.format(p.value.populationCount))
    editable = false
    prefWidth <== populationTable.width * 0.20
  }

  private val populationTypeColumn = new TableColumn[Population, String] {
    text = Localization("type")
    cellValueFactory = p => StringProperty(p.value.populationType.name)
    editable = false
    prefWidth <== populationTable.width * 0.25
  }

  private val populationClassColumn = new TableColumn[Population, String] {
    text = Localization("class")
    cellValueFactory = p => StringProperty(p.value.populationType.populationClass.name)
    editable = false
    prefWidth <== populationTable.width * 0.10
  }

  private val hapinessFormatter = new DecimalFormat("#00.00")
  private val hapinessColumn = new TableColumn[Population, String] {
    text = Localization("happiness")
    cellValueFactory = p => StringProperty(hapinessFormatter.format(p.value.happiness * 100) + "%")
    editable = false
    prefWidth <== populationTable.width * 0.144
  }

  populationTable.columns ++= List(populationClassColumn, populationTypeColumn,
    raceColumn, cultureColumn, populationCountColumn, hapinessColumn)

  private val buffer = new ObservableBuffer[Population]()
  buffer.addAll(regionPopulation.pops.sortBy(p => (p.populationType, p.populationCount)).reverse.asJava)
  populationTable.items = buffer

  populationTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  val selectedRow:ReadOnlyObjectProperty[Population] = populationTable.delegate.getSelectionModel.selectedItemProperty
  populationTable.delegate.getSelectionModel.clearAndSelect(0)
  add(populationTable, "growx,growy,pushx,pushy")
}

class PopDetailsPane(populationProperty: ReadOnlyObjectProperty[Population], province: Province) extends MigPane with WorldInterfaceJavaNode {
  import mr.merc.util.MercUtils.PropertyBindingMap

  add(new BigText{
    text <== populationProperty.map { p =>
      Localization("population.title", p.culture.race.name, p.culture.name, p.populationType.name, province.name)
    }
  }.delegate, "span,center")

  private val generalInfoPane = new TitledPane() {
    text = Localization("generalInfo")
    style = s"-fx-font-size: ${Components.largeFontSize}"
    content = new MigPane() {
      add(MediumText(Localization("population.populationCount")),"")
      add(new MediumText{
        private val formatter = new DecimalFormat()
        formatter.setGroupingSize(3)
        formatter.setGroupingUsed(true)
        text <== populationProperty.map(p => formatter.format(p.populationCount))
      }.delegate, "wrap")

      add(MediumText(Localization("population.literacy")),"")
      add(new MediumText{
        private val formatter = new DecimalFormat("#0.00")
        text <== populationProperty.map(p => formatter.format(p.literacy * 100) + "%")
      }.delegate, "wrap")

      add(MediumText(Localization("population.hapiness")))
      add(new MediumText{
        private val formatter = new DecimalFormat("#00.00")
        text <== populationProperty.map(p => formatter.format(p.happiness * 100) + "%")
      }.delegate, "wrap")

      private val moneyFormatter = new DecimalFormat("0.00")
      add(MediumText(Localization("population.moneyReserves")),"")
      add(new MediumText{
        text <== populationProperty.map(p => moneyFormatter.format(p.moneyReserves))
      }.delegate, "wrap")

      add(MediumText(Localization("population.netSalary")))
      add(new MediumText{
        text <== populationProperty.map(p => p.salary(0).headOption.
          map(h => moneyFormatter.format(h.receivedMoney)).getOrElse(""))
      }.delegate, "wrap")

      add(MediumText(Localization("population.taxes")))
      add(new MediumText{
        text <== populationProperty.map(p => p.salary(0).headOption.
          map(h => moneyFormatter.format(h.taxes)).getOrElse(""))
      }.delegate, "wrap")
    }
  }

  private val politicalViewsPane = new TitledPane() {
    text = Localization("politicalViews")
    style = s"-fx-font-size: ${Components.largeFontSize}"
    content = new MigPane("fill") {
      add(popToPie(_.foreignPolicy, ForeignPolicy), "span 1,growx,growy")
      add(popToPie(_.votersPolicy, VotersPolicy), "span 1,growx,growy")
      add(popToPie(_.economy, Economy), "span 1,wrap,growx,growy")
      add(popToPie(_.socialPolicy, SocialPolicy), "span 1,growx,growy")
      add(popToPie(_.migration, Migration), "span 1,growx,growy")
      add(popToPie(_.regime, Regime), "span 1, wrap,growx,growy")
    }
  }

  private val accordion = new Accordion()

  accordion.panes = List(generalInfoPane, politicalViewsPane)
  accordion.expandedPane = generalInfoPane

  add(accordion, "growx,growy,pushx,pushy")

  private def popToPie[T <: IssuePosition](f:PoliticalViews => IssuePositionPopularity[T], issue:Issue[T]):Pane = {
    val pane = new BorderPane()

    def reload(): Unit = {
      pane.center = politicalPointsToPie(f(populationProperty.value.politicalViews), issue)
    }

    populationProperty.onChange {
      reload()
    }
    reload()

    pane
  }

  private def politicalPointsToPie[T <: IssuePosition](position: IssuePositionPopularity[T], issue:Issue[T]):Node = {
    val percentFormatter = new DecimalFormat("#0.00")
    val pies = position.popularity.map {case (k, v) => PiePart(k.color, k.name, v * 100, Some(k.name + " " + percentFormatter.format(v * 100) + "%"))}.toList
    val chart = PieChartBuilder.build(pies)
    chart.title = Localization(issue.name)
    chart
  }

}