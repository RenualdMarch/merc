package mr.merc.ui.world

import com.sun.javafx.charts.Legend
import mr.merc.util.MercUtils
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Side
import scalafx.scene.chart.PieChart
import scalafx.scene.paint.Color

import scala.collection.JavaConverters._
import scalafx.Includes._

object PieChartBuilder {

  def build(pies:List[PiePart]): PieChart = {
    val piesForChart = pies.map { p =>
      PieChart.Data(p.label, p.count)
    }

    val chart = PieChart(ObservableBuffer(piesForChart))
    chart.labelsVisible = false
    chart.legendSide = Side.Bottom

    pies.zip(Stream.from(0)).foreach { case (p, i) =>
      chart.lookupAll(s".data$i").asScala.foreach { node =>
        node.style = s"-fx-pie-color:${MercUtils.colorToStyle(p.color)};"
      }
      val items = chart.delegate.lookupAll(".chart-legend").asScala.collect { case e:Legend =>
        e.getItems.asScala.find(_.getText == p.label)
      }
      items.flatten.foreach { li =>
        li.getSymbol.setStyle(s"-fx-pie-color: ${MercUtils.colorToStyle(p.color)};")
      }
    }

    chart.style = s"-fx-font-size: ${Components.smallFontSize}"
    chart
  }

  case class PiePart(color: Color, label: String, count: Double)
}


