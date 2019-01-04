package mr.merc.ui.world

import java.text.DecimalFormat

import javafx.scene.control.{SelectionMode, TableCell}
import mr.merc.economics._
import mr.merc.local.Localization
import mr.merc.politics.Province
import org.kordamp.ikonli.javafx.FontIcon
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.layout.Pane
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.Scene
import scalafx.scene.chart.{AreaChart, LineChart, NumberAxis, XYChart}
import scalafx.stage.{Modality, Stage}

import scala.collection.JavaConverters._

class MarketViewPane(province: Province, stage: Stage) extends PaneWithTwoEqualHorizontalChildren {
  val productsTablePane = new ProductsTablePane(province.regionMarket)
  val productPane = new ProductPane(productsTablePane.selectedItem, province, stage)

  setTwoChildren(productsTablePane, productPane)
}

class ProductsTablePane(market: RegionMarket) extends MigPane with WorldInterfaceJavaNode {
  private val format = new DecimalFormat("#0.00")

  private val productsTable = new TableView[ProductsInfo]()
  productsTable.style = s"-fx-font-size: ${Components.mediumFontSize}"

  private val productNameColumn = new TableColumn[ProductsInfo, String] {
    text = Localization("productName")
    cellValueFactory = p => StringProperty(Localization(p.value.product.name))
    editable = false
    prefWidth <== productsTable.width * 0.15
  }

  private val priceColumn = new TableColumn[ProductsInfo, String] {
    text = Localization("price")
    cellValueFactory = p => StringProperty(p.value.currentPrice.map(format.format).getOrElse(""))
    editable = false
    prefWidth <== productsTable.width * 0.15
  }

  private val upOrDownSymbol = new TableColumn[ProductsInfo, FontIcon] {
    text = Localization("tendency")
    cellFactory = p => new TableCell[ProductsInfo, FontIcon] {
      override def updateItem(t: FontIcon, b: Boolean): Unit = {
        super.updateItem(t, b)
        Option(t).foreach { opt =>
          opt.setIconSize(30)
        }

        setGraphic(t)
      }
    }
    cellValueFactory = p => ObjectProperty[FontIcon](p.value.priceDiffImage)
    editable = false
    prefWidth <== productsTable.width * 0.15
  }

  private val demandColumn = new TableColumn[ProductsInfo, String] {
    text = Localization("demand")
    cellValueFactory = p => StringProperty(p.value.totalDemand.map(format.format).getOrElse(""))
    editable = false
    prefWidth <== productsTable.width * 0.15
  }

  private val supplyColumn = new TableColumn[ProductsInfo, String] {
    text = Localization("supply")
    cellValueFactory = p => StringProperty(p.value.totalSupply.map(format.format).getOrElse(""))
    editable = false
    prefWidth <== productsTable.width * 0.15
  }

  productsTable.columns ++= List(productNameColumn, priceColumn, upOrDownSymbol, demandColumn, supplyColumn)

  private val buffer = new ObservableBuffer[ProductsInfo]()
  buffer.addAll(market.history.map(ProductsInfo.apply _ tupled).toList.asJava)
  productsTable.items = buffer


  productsTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  productsTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[ProductsInfo] = productsTable.delegate.getSelectionModel.selectedItemProperty
  add(productsTable, "growx,growy,pushx,pushy")
}

class ProductPane(productProperty: ReadOnlyObjectProperty[ProductsInfo], province: Province, stage: Stage) extends Pane with WorldInterfaceNode {

  def reload(): Unit = {
    val pane: Pane = new ProductDetailPane(productProperty.value, province, stage)
    this.children.clear()
    this.children.add(pane)
    pane.prefWidth <== this.width
    pane.prefHeight <== this.height
  }

  productProperty.onChange(reload())

  reload()
}

class ProductDetailPane(productsInfo: ProductsInfo, province: Province, stage: Stage) extends MigPane {

  def buildPriceChart(): Option[LineChart[Number, Number]] = {
    val size = productsInfo.history.size
    productsInfo.history.headOption.map { first =>
      val xAxis = new NumberAxis(first.turn, first.turn + size, 1)
      val yAxis = new NumberAxis()
      xAxis.label = Localization("weeks")
      yAxis.label = Localization("price")

      val lineChart = new LineChart[Number, Number](xAxis, yAxis)
      lineChart.title = Localization("priceChange")
      lineChart.style = s"-fx-font-size: ${Components.mediumFontSize}"

      val series = new XYChart.Series[Number, Number]()
      productsInfo.history.foreach { h =>
        series.getData.add(XYChart.Data[Number, Number](h.turn, h.price))
      }

      lineChart.legendVisible = false
      lineChart.getData.add(series)

      lineChart.lookupAll(".chart-line-symbol").asScala.foreach { s =>
        val dataNumber = s.getStyleClass.find(_.startsWith("data")).get.replace("data", "").toInt + first.turn
        s.onMouseClicked = _ => showSupplyDemandDialog(productsInfo.history(dataNumber - 1), province)
      }

      lineChart
    }
  }

  def buildSupplyDemandChart(): Option[AreaChart[Number, Number]] = {
    val size = productsInfo.history.size
    productsInfo.history.headOption.map { first =>
      val xAxis = NumberAxis(first.turn, first.turn + size, 1)
      val yAxis = NumberAxis()
      xAxis.label = Localization("weeks")
      yAxis.label = Localization("units")

      val chart = new AreaChart[Number, Number](xAxis, yAxis)
      chart.title = Localization("supplyDemandChart")
      chart.style = s"-fx-font-size: ${Components.mediumFontSize}"

      val demandSeries = productsInfo.history.foldLeft(new XYChart.Series[Number, Number]()) {
        case (series, day) =>
          series.getData.add(XYChart.Data[Number, Number](day.turn, day.totalDemand))
          series
      }
      demandSeries.name = Localization("demand")

      val supplySeries = productsInfo.history.foldLeft(new XYChart.Series[Number, Number]()) {
        case (series, day) =>
          series.getData.add(XYChart.Data[Number, Number](day.turn, day.totalSupply))
          series
      }
      supplySeries.name = Localization("supply")

      chart.getData.addAll(demandSeries, supplySeries)

      chart.lookupAll(".chart-area-symbol").asScala.foreach {s =>
        val dataNumber = s.getStyleClass.find(_.startsWith("data")).get.replace("data", "").toInt + first.turn
        s.onMouseClicked = _ => showSupplyDemandDialog(productsInfo.history(dataNumber - 1), province)
      }

      chart
    }
  }

  def showSupplyDemandDialog(day: MarketDay, province: Province): Unit = {
    val pane  = buildSupplyDemandDayDialogPane(day, province)

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

  def buildSupplyDemandDayDialogPane(day: MarketDay, province: Province):Pane = {
    val pane = new MigPane()

    val supplyTitle = BigText(Localization("supply"))
    val demandTitle = BigText(Localization("demand"))
    val supplyTable = SupplyDemandTables.buildSupplyTable(day)
    val demandTable = SupplyDemandTables.buildDemandTable(day, province)

    pane.add(supplyTitle, "span, center, wrap")
    pane.add(supplyTable, "span, growx, growx, pushx, pushy, wrap")
    pane.add(demandTitle, "span, center, wrap")
    pane.add(demandTable, "span, growx, growx, pushx, pushy, wrap")

    pane
  }

  add(buildPriceChart().getOrElse(new Pane()), "growx,growy,pushx,pushy,wrap")
  add(buildSupplyDemandChart().getOrElse(new Pane()), "growx,growy,pushx,pushy")
}

case class ProductsInfo(product: Products.Product, history: Vector[MarketDay]) {

  private def priceDiff: Option[Double] = {
    history.takeRight(2) match {
      case Vector(beforeLast, last) => Some(last.price - beforeLast.price)
      case _ => None
    }
  }

  def currentPrice: Option[Double] = history.lastOption.map(_.price)

  def totalDemand: Option[Double] = history.lastOption.map(_.totalDemand)

  def totalSupply: Option[Double] = history.lastOption.map(_.totalSupply)

  def priceDiffImage: FontIcon = {
    priceDiff match {
      case None =>
        new FontIcon("fa-circle-o")
      case Some(x) if x > 0 => new FontIcon("fa-arrow-circle-o-up")
      case Some(x) if x == 0 => new FontIcon("fa-arrow-circle-right")
      case Some(x) if x < 0 => new FontIcon("fa-arrow-circle-o-down")
    }
  }
}