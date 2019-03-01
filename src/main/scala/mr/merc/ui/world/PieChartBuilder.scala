package mr.merc.ui.world

import com.sun.javafx.charts.Legend
import javafx.scene.Node
import mr.merc.util.{MercTooltip, MercUtils}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Pos, Side}
import scalafx.scene.chart.PieChart
import scalafx.scene.paint.Color

import scala.collection.JavaConverters._
import scalafx.Includes._
import scalafx.scene.control.{ScrollPane, TableColumn, TableView}
import scalafx.scene.layout.{BorderPane, Region}
import scalafx.scene.shape.{Circle, Ellipse, Rectangle}

object PieChartBuilder {

  def build(pies: List[PiePart]): PieChart = {
    val piesForChart = pies.map { p =>
      PieChart.Data(p.label, p.count)
    }

    val chart = PieChart(ObservableBuffer(piesForChart))
    chart.labelsVisible = false
    chart.legendSide = Side.Bottom

    pies.zipWithIndex.foreach { case (p, i) =>
      chart.lookupAll(s".data$i").asScala.foreach { node: Node =>
        node.style = s"-fx-pie-color:${MercUtils.colorToStyle(p.color)};"

        p.tooltip.foreach { tooltip =>
          MercTooltip.applyTooltip(node, tooltip)
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

  def buildPieWithScrollableLegend(pies: List[PiePart], legendSide: Side, title: Option[String] = None): Region = {
    val chart = build(pies)
    val legend = buildLegend(pies.sortBy(-_.count))
    chart.legendVisible = false
    title.foreach { t =>
      chart.title = t
    }
    legendSide match {
      case Side.Bottom =>
        PaneWithTwoEqualVerticalChildren(chart, legend)
      case Side.Top =>
        PaneWithTwoEqualVerticalChildren(legend, chart)
      case Side.Left =>
        PaneWithTwoEqualHorizontalChildren(legend, chart)
      case Side.Right =>
        PaneWithTwoEqualHorizontalChildren(chart, legend)
    }
  }

  def buildLegend(pies: List[PiePart]): Region = {

    val scrollPane = new ScrollPane()
    val innerPane = new MigPane()
    pies.map(buildLegendRow).foreach {p =>
      innerPane.add(p, "grow,push,wrap")
    }
    scrollPane.content = innerPane
    scrollPane.fitToWidth = true
    scrollPane
  }

  private def buildLegendRow(part: PiePart):Region = {
    val borderPane = new BorderPane()
    val square = new Circle()
    square.radius <== borderPane.height / 2
    square.fill = part.color
    val text = MediumText(part.label)
    val percentage = MediumText(DoubleFormatter().format(part.count) + "%")

    borderPane.left = square
    borderPane.center = text
    borderPane.right = percentage

    BorderPane.setAlignment(text, Pos.CenterLeft)

    borderPane
  }

  case class PiePart(color: Color, label: String, count: Double, tooltip: Option[String])

}


