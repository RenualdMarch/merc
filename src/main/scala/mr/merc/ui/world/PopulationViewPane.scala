package mr.merc.ui.world

import javafx.scene.control.SelectionMode
import mr.merc.economics.Population.{Lower, Middle, Upper}
import mr.merc.economics.{Population, RegionPopulation}
import mr.merc.local.Localization
import mr.merc.politics._
import mr.merc.ui.world.PieChartBuilder.PiePart
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.layout.Pane
import scalafx.Includes._
import scalafx.beans.property.{ReadOnlyObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.control._
import EconomicLocalization._
import scalafx.geometry.Side
import mr.merc.util.FxPropertyUtils._
import scalafx.scene.control.TabPane.TabClosingPolicy

import scala.collection.JavaConverters._

class PopulationViewPane(province: Province) extends PaneWithTwoHorizontalChildren {
  val popsTablePane = new PopsTablePane(province.regionPopulation, province)
  val popDetailsPane: Pane = new PopDetailsPane(popsTablePane.selectedRow, province)

  setTwoChildren(popsTablePane, popDetailsPane)
}

class PopsTablePane(regionPopulation: RegionPopulation, province: Province) extends MigPane with WorldInterfaceJavaNode {
  private val populationTable = new TableView[PopulationInfo]()
  populationTable.style = Components.mediumFontStyle

  private val raceColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("race")
    cellValueFactory = p => StringProperty(p.value.race)
    editable = false
    prefWidth <== populationTable.width * 0.15
  }

  private val cultureColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("culture")
    cellValueFactory = p => StringProperty(p.value.culture)
    editable = false
    prefWidth <== populationTable.width * 0.15
  }

  private val populationCountColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("population")
    cellValueFactory = p => StringProperty(p.value.populationCount)
    editable = false
    prefWidth <== populationTable.width * 0.20
  }

  private val populationTypeColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("type")
    cellValueFactory = p => StringProperty(p.value.populationType)
    editable = false
    prefWidth <== populationTable.width * 0.25
  }

  private val populationClassColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("class")
    cellValueFactory = p => StringProperty(p.value.populationClass)
    editable = false
    prefWidth <== populationTable.width * 0.10
  }

  private val happinessColumn = new TableColumn[PopulationInfo, String] {
    text = Localization("happiness")
    cellValueFactory = p => StringProperty(p.value.consumptionHappiness)
    editable = false
    prefWidth <== populationTable.width * 0.144
  }

  populationTable.columns ++= List(populationClassColumn, populationTypeColumn,
    raceColumn, cultureColumn, populationCountColumn, happinessColumn)

  private val buffer = new ObservableBuffer[PopulationInfo]()

  private val info = regionPopulation.pops.sortBy(p => (p.populationType, p.populationCount)).map(p => new PopulationInfo(List(p), province))

  buffer.add(new PopulationInfo(regionPopulation.pops, province))
  buffer.add(new PopulationInfo(regionPopulation.pops.filter(_.populationType.populationClass == Upper), province))
  buffer.add(new PopulationInfo(regionPopulation.pops.filter(_.populationType.populationClass == Middle), province))
  buffer.add(new PopulationInfo(regionPopulation.pops.filter(_.populationType.populationClass == Lower), province))
  buffer.addAll(info.reverse.asJava)
  populationTable.items = buffer

  populationTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  populationTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedRow:ReadOnlyObjectProperty[PopulationInfo] = populationTable.delegate.getSelectionModel.selectedItemProperty
  add(populationTable, "grow,push")
}

class PopDetailsPane(populationProperty: ReadOnlyObjectProperty[PopulationInfo], province: Province) extends MigPane with WorldInterfaceJavaNode {

  add(new BigText{
    text <== populationProperty.map (p => p.pageTitle)
  }.delegate, "span,center")

  private val generalInfoPane = new Tab() {
    text = Localization("generalInfo")
    style = Components.largeFontStyle
    content = new MigPane() {
      add(MediumText(Localization("population.populationCount")),"")
      add(new MediumText{
        text <== populationProperty.map(_.populationCount)
      }.delegate, "wrap")

      add(MediumText(Localization("population.literacy")),"")
      add(new MediumText{
        text <== populationProperty.map(_.literacy)
      }.delegate, "wrap")

      add(MediumText(Localization("population.consumptionHappiness")))
      add(new MediumText{
        text <== populationProperty.map(_.consumptionHappiness)
      }.delegate, "wrap")

      add(MediumText(Localization("population.politicalHappiness")))
      add(new MediumText{
        text <== populationProperty.map(_.politicalHappiness)
      }.delegate, "wrap")

      add(MediumText(Localization("population.growth")))
      add(new MediumText{
        text <== populationProperty.map(_.growth)
      }.delegate, "wrap")

      add(MediumText(Localization("population.moneyReserves")),"")
      add(new MediumText{
        text <== populationProperty.map(_.moneyReserves)
      }.delegate, "wrap")

      add(MediumText(Localization("population.netSalary")))
      add(new MediumText{
        text <== populationProperty.map(_.netSalary)
      }.delegate, "wrap")

      add(MediumText(Localization("population.taxes")))
      add(new MediumText{
        text <== populationProperty.map(_.taxes)
      }.delegate, "wrap")
    }
  }

  private val politicalViewsPane = new Tab() {
    text = Localization("politicalViews")
    style = Components.largeFontStyle
    content = new MigPane("fill") {
      add(popToPie(_.sumViews.foreignPolicy, ForeignPolicy), "span 1,grow,push")
      add(popToPie(_.sumViews.votersPolicy, VotersPolicy), "span 1,grow,push")
      add(popToPie(_.sumViews.economy, Economy), "span 1,wrap,grow,push")
      add(popToPie(_.sumViews.socialPolicy, SocialPolicy), "span 1,grow,push")
      add(popToPie(_.sumViews.migration, Migration), "span 1,grow,push")
      add(popToPie(_.sumViews.regime, Regime), "span 1, wrap,grow,push")
    }
  }



  private val tabPane = new TabPane() {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  tabPane.tabs = List(generalInfoPane, politicalViewsPane, populationNeedsPane())

  add(tabPane, "grow,push")

  private def populationNeedsPane():Tab = new Tab() {
    content = new PropertyDependentPane[PopulationInfo](populationProperty,
      p => SupplyDemandTables.buildPopulationDemandTable(p.population.flatMap(_.needsFulfillment(1).headOption)))

    text = Localization("needs")
    style = s"-fx-font-size: ${Components.largeFontSize}"
  }

  private def popToPie[T <: IssuePosition](f:PopulationInfo => Map[T, Double], issue:Issue[T]):Pane =
    new PropertyDependentPane[PopulationInfo](populationProperty, p => politicalPointsToPie(f(p), issue))


  private def politicalPointsToPie[T <: IssuePosition](position: Map[T, Double], issue:Issue[T]):Node = {
    val pies = position.map {case (k, v) => PiePart(k.color, k.name, v * 100, Some(k.name + " " + DoubleFormatter().format(v * 100) + "%"))}.toList
    val chart = PieChartBuilder.buildPieWithScrollableLegend(pies, Side.Bottom, Some(Localization(issue.name)))
    chart
  }

}

class PopulationInfo(val population:List[Population], province: Province) {

  def race: String = {
    val possibleRace = population.head.culture.race
    if (population.forall(_.culture.race == possibleRace)) {
      Localization(possibleRace.name)
    } else Localization("population.all")
  }

  def culture: String = {
    val possibleCulture = population.head.culture
    if (population.forall(_.culture == possibleCulture)) {
      Localization(possibleCulture.name)
    } else Localization("population.all")
  }

  def populationCount: String = {
    IntFormatter().format(population.map(_.populationCount).sum)
  }

  def populationType: String = {
    val possibleType = population.head.populationType
    if (population.forall(_.populationType == possibleType)) {
      Localization(possibleType.name)
    } else Localization("population.all")
  }

  def populationClass: String = {
    val possibleClass = population.head.populationType.populationClass
    if (population.forall(_.populationType.populationClass == possibleClass)) {
      Localization(possibleClass.name)
    } else Localization("population.all")
  }

  def consumptionHappiness: String = {
    IntFormatter().format(population.map(p => (p.populationCount, p.consumptionHappiness)).reduce[(Int, Double)] {
      case ((c1, h1),(c2, h2)) => (c1 + c2, (c1 * h1 + c2 * h2) / (c1 + c2))
    }._2 * 100)  + "%"
  }

  def politicalHappiness: String = {
    IntFormatter().format(population.map(p => (p.populationCount, p.politicalHappiness(province.owner))).reduce[(Int, Double)] {
      case ((c1, h1),(c2, h2)) => (c1 + c2, (c1 * h1 + c2 * h2) / (c1 + c2))
    }._2 * 100)  + "%"
  }

  def growth:String = {
    val grown = population.map(p => p.populationCount * (1 + p.growthRate)).sum
    val notGrown = population.map(_.populationCount).sum
    val r = (grown / notGrown - 1) * 100
    DoubleFormatter().format(r) + "%"
  }

  def literacy: String = {
    val lit = population.map(_.literateCount).sum
    val all = population.map(_.populationCount).sum
    DoubleFormatter().format(lit * 100 / all.toDouble) + "%"
  }

  def moneyReserves: String = {
    val t = population.map(_.moneyReserves).sum
    DoubleFormatter().format(t)
  }

  def netSalary: String = {
    val t = population.flatMap(_.salary.lastOption.map(_.receivedMoney)).sum
    DoubleFormatter().format(t)
  }

  def taxes: String = {
    val t = population.flatMap(_.salary.lastOption.map(_.taxes)).sum
    DoubleFormatter().format(t)
  }

  val sumViews: CurrentPoliticalViews = {
    import mr.merc.economics.MapUtil.FloatOperations._
    population.map { p =>
      p.populationCount -> p.politicalViews.currentViews(p.literacy)
    }.reduce[(Int, CurrentPoliticalViews)] { case ((c1, pv1), (c2, pv2)) =>
      (c1 + c2) -> CurrentPoliticalViews(
        ((pv1.migration |*| c1) |+| (pv2.migration |*| c2)).scaleToSum(1d),
        ((pv1.regime |*| c1) |+| (pv2.regime |*| c2)).scaleToSum(1d),
        ((pv1.foreignPolicy |*| c1) |+| (pv2.foreignPolicy |*| c2)).scaleToSum(1d),
        ((pv1.economy |*| c1) |+| (pv2.economy |*| c2)).scaleToSum(1d),
        ((pv1.socialPolicy |*| c1) |+| (pv2.socialPolicy |*| c2)).scaleToSum(1d),
        ((pv1.votersPolicy |*| c1) |+| (pv2.votersPolicy |*| c2)).scaleToSum(1d)
      )
    }._2
  }

  def pageTitle: String = {
    population match {
      case List(p) => localizePopulation(p, province)
      case x :: _ if population.forall(_.populationType.populationClass == Upper) => Localization("population.title.upper", x.culture.race.name, x.culture.name, province.name)
      case x :: _ if population.forall(_.populationType.populationClass == Middle) => Localization("population.title.middle", x.culture.race.name, x.culture.name, province.name)
      case x :: _ if population.forall(_.populationType.populationClass == Lower) => Localization("population.title.lower", x.culture.race.name, x.culture.name, province.name)
      case _ => Localization("population.title.all", province.name)
    }
  }
}