package mr.merc.ui.world

import com.sun.javafx.charts.Legend
import javafx.scene.Node
import mr.merc.util.MercUtils
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Side
import scalafx.scene.chart.PieChart
import scalafx.scene.paint.Color

import scala.collection.JavaConverters._
import scalafx.Includes._
import scalafx.scene.control.Label

object PieChartBuilder {

  def build(pies: List[PiePart]): PieChart = {
    val piesForChart = pies.map { p =>
      PieChart.Data(p.label, p.count)
    }

    val chart = PieChart(ObservableBuffer(piesForChart))
    chart.labelsVisible = false
    chart.legendSide = Side.Bottom

    val label = new Label()
    label.stylesheets.add("/css/tooltip.css")
    label.styleClass.add("tooltip")
    label.style = s"-fx-font-size: ${Components.mediumFontSize}"
    label.setMouseTransparent(true)

    pies.zipWithIndex.foreach { case (p, i) =>
      chart.lookupAll(s".data$i").asScala.foreach { node: Node =>
        node.style = s"-fx-pie-color:${MercUtils.colorToStyle(p.color)};"

        p.tooltip.foreach { tooltip =>
          node.onMouseEntered = { _ =>
            node.getScene.getChildren.remove(label)
            val bounds = node.localToScene(node.getBoundsInLocal)
            label.text = tooltip
            label.layoutX = bounds.getCenterX
            label.layoutY = bounds.getCenterY
            node.getScene.getChildren.add(label)
          }

          node.onMouseExited = { _ =>
            node.getScene.getChildren.remove(label)
          }
        }

      }
      val items = chart.delegate.lookupAll(".chart-legend").asScala.collect { case e: Legend =>
        e.getItems.asScala.find(_.getText == p.label)
      }
      items.flatten.foreach { li =>
        li.getSymbol.setStyle(s"-fx-pie-color: ${MercUtils.colorToStyle(p.color)};")
      }
    }

    chart.style = s"-fx-font-size: ${Components.smallFontSize}"
    chart
  }

  case class PiePart(color: Color, label: String, count: Double, tooltip: Option[String])

}


