package mr.merc.ui.world

import mr.merc.local.Localization
import mr.merc.log.Logging
import mr.merc.politics.{Province, State}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.scene.Node
import scalafx.scene.paint.Color

class ProvinceDetailsPane(province: Province, parent: WorldFrame) extends MigPane("", "") with Logging {

    add(BigText(province.name), "span,center,wrap")
    add(MediumText(Localization("owner")))
    add(new StatePropertyNode(province.owner), "wrap")
    add(MediumText(Localization("populationCount")))
    add(populationCountText, "wrap")
    add(MediumText(Localization("bureaucratsPercentage")), "")
    add(MediumText(DoubleFormatter().format(province.bureaucratsPercentageFromMax)), "wrap")
    add(MediumText(Localization("gdp")))
    add(MediumText(IntFormatter().format(province.gpd)), "wrap")
    add(MediumText(Localization("cultures")), "span,center,wrap")
    add(culturesDiagram, "span,wrap")
    add(new MigPane("", "") {
      val populationViewButton = MediumButton(Localization("population"))
      populationViewButton.onAction = _ => parent.showPopulationPane(province)
      val factoriesViewButton = MediumButton(Localization("production"))
      factoriesViewButton.onAction = _ => parent.showEnterprisesPane(province)
      val armyViewButton = MediumButton(Localization("army"))
      armyViewButton.onAction = _ => parent.showArmyMovement(province)
      val marketViewButton = MediumButton(Localization("market"))
      marketViewButton.onAction = _ => parent.showMarket(province)
      add(populationViewButton, "pushx,growx")
      add(factoriesViewButton, "pushx,growx,wrap")
      add(armyViewButton, "pushx,growx")
      add(marketViewButton, "pushx,growx,wrap")
    }, "span,growx")


  private def populationCountText: Text = {
    val str = IntFormatter().format(province.totalPopulation)
    MediumText(str)
  }

  private def culturesDiagram: Node = {
    val pies = province.regionPopulation.cultureMembers.map { case (culture, count) =>
      PieChartBuilder.PiePart(culture.color, Localization(culture.cultureNameKey), count, Some(Localization(culture.cultureNameKey)))
    }.toList

    PieChartBuilder.build(pies)
  }
}

class StatePropertyNode(state: State) extends MigPane("") {
  private val colorSquare = new Rectangle()
  colorSquare.layoutX = 0
  colorSquare.layoutY = 0
  colorSquare.height = 20
  colorSquare.width = 20
  colorSquare.fill = state.color
  colorSquare.stroke = Color.Black
  private val name = MediumText(state.name)
  this.add(colorSquare)
  this.add(name)
}
