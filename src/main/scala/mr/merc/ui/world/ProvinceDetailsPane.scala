package mr.merc.ui.world

import java.text.DecimalFormat

import com.sun.javafx.charts.Legend
import mr.merc.local.Localization
import mr.merc.log.Logging
import mr.merc.politics.{Province, State}
import mr.merc.util.MercUtils
import org.tbee.javafx.scene.layout.MigPane
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Side
import scalafx.scene.chart.PieChart
import scalafx.scene.layout._
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.Includes._
import scalafx.scene.paint.Color

import scala.collection.JavaConverters._

class ProvinceDetailsPane(province: Province, parent: WorldFrame) extends MigPane("", "") with Logging {

    add(BigText(province.name), "span,center,wrap")
    add(SmallText(Localization("owner")))
    add(new StatePropertyNode(province.owner), "wrap")
    add(SmallText(Localization("populationCount")))
    add(populationCountText, "wrap")
    add(SmallText(Localization("cultures")), "span,center,wrap")
    add(culturesDiagram, "span,wrap")
    add(new MigPane("", "") {
      val populationViewButton = SmallButton(Localization("population"))
      populationViewButton.onAction = _ => parent.showPopulationPane(province)
      val factoriesViewButton = SmallButton(Localization("factories"))
      val armyViewButton = SmallButton(Localization("army"))
      add(populationViewButton, "pushx,growx,wrap")
      add(factoriesViewButton, "pushx,growx,wrap")
      add(armyViewButton, "pushx,growx,wrap")
    }, "span,growx")


  private def populationCountText: Text = {
    val format = new DecimalFormat()
    format.setGroupingSize(3)
    format.setGroupingUsed(true)
    val str = format.format(province.totalPopulation)
    SmallText(str)
  }

  private def culturesDiagram: PieChart = {
    val pies = province.regionPopulation.cultureMembers.map { case (culture, count) =>
      PieChartBuilder.PiePart(culture.color, Localization(culture.cultureNameKey), count)
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
  private val name = SmallText(state.name)
  this.add(colorSquare)
  this.add(name)
}
