package mr.merc.ui.world

import java.text.DecimalFormat

import mr.merc.economics.Factory.FactoryRecord
import mr.merc.economics._
import mr.merc.economics.Population.{LifeNeeds, LuxuryNeeds, PopulationNeedsType, ProductFulfillmentRecord, RegularNeeds}
import mr.merc.local.Localization
import scalafx.beans.property.StringProperty
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.layout.Region
import EconomicLocalization._
import mr.merc.economics.ResourceGathering.ResourceGatheringRecord
import mr.merc.politics.Province

import scala.collection.JavaConverters._

object SupplyDemandTables {
  private val format = new DecimalFormat("#0.00")

  def buildSupplyTable(day: MarketDay): Region = {
    case class Supply(source: String, brought: Double, sold: Double, price: Double)

    val table = new TableView[Supply]()
    table.style = s"-fx-font-size: ${Components.mediumFontSize}"

    val sourceColumn = new TableColumn[Supply, String] {
      text = Localization("source")
      cellValueFactory = e => StringProperty(e.value.source)
      editable = false
      prefWidth <== table.width * 0.57
    }

    val priceColumn = new TableColumn[Supply, String] {
      text = Localization("price")
      cellValueFactory = e => StringProperty(format.format(e.value.price))
      editable = false
      prefWidth <== table.width * 0.13
    }

    val broughtColumn = new TableColumn[Supply, String] {
      text = Localization("bought")
      cellValueFactory = e => StringProperty(format.format(e.value.brought))
      editable = false
      prefWidth <== table.width * 0.13
    }

    val soldColumn = new TableColumn[Supply, String] {
      text = Localization("sold")
      cellValueFactory = e => StringProperty(format.format(e.value.sold))
      editable = false
      prefWidth <== table.width * 0.13
    }

    table.columns.addAll(sourceColumn, priceColumn, broughtColumn, soldColumn)

    val supplies = day.fulfilledSupply.get.map { r =>
      val name = r.request match {
        case EnterpriseSupplyRequest(enterprise, _, _) => localizeEnterprise(enterprise,
          enterprise.region.asInstanceOf[Province])
      }
      Supply(name, r.sold, r.request.count, r.price)
    }

    val buffer = new ObservableBuffer[Supply]()
    buffer.addAll(supplies.asJava)

    table.items = buffer

    table
  }

  def buildDemandTable(day: MarketDay, province: Province): Region = {
    case class Demand(source: String, needed: Double, bought: Double, price: Double)


    val table = new TableView[Demand]()
    table.style = s"-fx-font-size: ${Components.mediumFontSize}"

    val sourceColumn = new TableColumn[Demand, String] {
      text = Localization("source")
      cellValueFactory = e => StringProperty(e.value.source)
      editable = false
      prefWidth <== table.width * 0.57
    }

    val priceColumn = new TableColumn[Demand, String] {
      text = Localization("price")
      cellValueFactory = e => StringProperty(format.format(e.value.price))
      editable = false
      prefWidth <== table.width * 0.13
    }

    val neededColumn = new TableColumn[Demand, String] {
      text = Localization("needed")
      cellValueFactory = e => StringProperty(format.format(e.value.needed))
      editable = false
      prefWidth <== table.width * 0.13
    }

    val boughtColumn = new TableColumn[Demand, String] {
      text = Localization("bought")
      cellValueFactory = e => StringProperty(format.format(e.value.bought))
      editable = false
      prefWidth <== table.width * 0.13
    }

    table.columns.addAll(sourceColumn, priceColumn, neededColumn, boughtColumn)

    val demands = day.fulfilledDemands.get.map { r =>
      r.request match {
        case EnterpriseDemandRequest(enterprise, _, count) =>
          Demand(localizeEnterprise(enterprise, province), count, r.bought, r.price)
        case PopulationDemandRequest(pop, _, count) =>
          Demand(localizePopulation(pop, province), count, r.bought, r.price)
      }
    }

    val buffer = new ObservableBuffer[Demand]()
    buffer.addAll(demands.asJava)

    table.items = buffer

    table
  }

  def buildPopulationDemandTable(records: List[ProductFulfillmentRecord]): Region = {
    case class PopDemand(product: Products.Product, needsType: PopulationNeedsType, demanded: Double, received: Double, price: Option[Double])

    val table = new TableView[PopDemand]()
    table.style = s"-fx-font-size: ${Components.mediumFontSize}"

    def extractDemands(needsType: PopulationNeedsType):List[PopDemand] = {
      records.flatMap { record =>
        record.needsFulfillmentInfo(needsType).map { a =>
          PopDemand(a.product, needsType, a.demanded, a.received, a.price)
        }
      }.groupBy(pd => pd.product).values.map { list =>
        PopDemand(list.head.product, list.head.needsType, list.map(_.demanded).sum, list.map(_.received).sum, list.head.price)
      }.toList
    }

    val items = extractDemands(LifeNeeds) ++ extractDemands(RegularNeeds) ++ extractDemands(LuxuryNeeds)

    val needsColumn = new TableColumn[PopDemand, String] {
      text = Localization("needsType")
      cellValueFactory = e => StringProperty(Localization(e.value.needsType.name))
      editable = false
      prefWidth <== table.width * 0.25
    }

    val productColumn = new TableColumn[PopDemand, String] {
      text = Localization("product")
      cellValueFactory = e => StringProperty(Localization(e.value.product.name))
      editable = false
      prefWidth <== table.width * 0.25
    }

    val priceColumn = new TableColumn[PopDemand, String] {
      text = Localization("price")
      cellValueFactory = e => StringProperty(e.value.price.map(format.format).getOrElse(""))
      editable = false
      prefWidth <== table.width * 0.15
    }

    val demandedColumn = new TableColumn[PopDemand, String] {
      text = Localization("demand")
      cellValueFactory = e => StringProperty(format.format(e.value.demanded))
      editable = false
      prefWidth <== table.width * 0.15
    }

    val receivedColumn = new TableColumn[PopDemand, String] {
      text = Localization("received")
      cellValueFactory = e => StringProperty(format.format(e.value.received))
      editable = false
      prefWidth <== table.width * 0.15
    }

    table.columns.addAll(needsColumn, productColumn, priceColumn, demandedColumn, receivedColumn)

    val buffer = new ObservableBuffer[PopDemand]()
    buffer.addAll(items.asJava)

    table.items = buffer

    table
  }

  def buildFactoryDemandTable(record: FactoryRecord): Region = {
    case class Row(product: Products.Product, price: Double, demanded: Double, bought: Double)

    val boughtRows = record.bought.map {f =>
      Row(f.request.product, f.price, f.bought, f.request.count)
    }

    val table = new TableView[Row]()
    table.style = s"-fx-font-size: ${Components.mediumFontSize}"

    val productColumn = new TableColumn[Row, String] {
      text = Localization("product")
      cellValueFactory = e => StringProperty(Localization(e.value.product.name))
      editable = false
      prefWidth <== table.width * 0.25
    }

    val priceColumn = new TableColumn[Row, String] {
      text = Localization("price")
      cellValueFactory = e => StringProperty(format.format(e.value.price))
      editable = false
      prefWidth <== table.width * 0.25
    }

    val demandedColumn = new TableColumn[Row, String] {
      text = Localization("demand")
      cellValueFactory = e => StringProperty(format.format(e.value.demanded))
      editable = false
      prefWidth <== table.width * 0.25
    }

    val boughtColumn = new TableColumn[Row, String] {
      text = Localization("received")
      cellValueFactory = e => StringProperty(format.format(e.value.bought))
      editable = false
      prefWidth <== table.width * 0.25
    }

    table.columns.addAll(productColumn, priceColumn, demandedColumn, boughtColumn)

    val buffer = new ObservableBuffer[Row]()
    buffer.addAll(boughtRows.asJava)

    table.items = buffer

    table
  }

  def buildEnterpriseSupplyTable(record: DayRecord): Region = {
    case class Row(product: Products.Product, region: String, price: Double, profit: Double, sent: Double, sold: Double)

    val supply = record.sold.map { case (r, s) =>
      Row(s.request.request.product, r.asInstanceOf[Province].name, s.request.price, s.profitPerItem, s.request.request.count, s.request.sold)
    }.toList


    val table = new TableView[Row]()
    table.style = s"-fx-font-size: ${Components.mediumFontSize}"

    val productColumn = new TableColumn[Row, String] {
      text = Localization("product")
      cellValueFactory = e => StringProperty(Localization(e.value.product.name))
      editable = false
      prefWidth <== table.width * 0.16
    }

    val regionColumn = new TableColumn[Row, String] {
      text = Localization("region")
      cellValueFactory = e => StringProperty(Localization(e.value.region))
      editable = false
      prefWidth <== table.width * 0.16
    }

    val priceColumn = new TableColumn[Row, String] {
      text = Localization("price")
      cellValueFactory = e => StringProperty(format.format(e.value.price))
      editable = false
      prefWidth <== table.width * 0.16
    }


    val profitColumn = new TableColumn[Row, String] {
      text = Localization("profit")
      cellValueFactory = e => StringProperty(format.format(e.value.profit))
      editable = false
      prefWidth <== table.width * 0.16
    }

    val sentColumn = new TableColumn[Row, String] {
      text = Localization("sent")
      cellValueFactory = e => StringProperty(format.format(e.value.sent))
      editable = false
      prefWidth <== table.width * 0.16
    }

    val soldColumn = new TableColumn[Row, String] {
      text = Localization("sold")
      cellValueFactory = e => StringProperty(format.format(e.value.sold))
      editable = false
      prefWidth <== table.width * 0.16
    }

    table.columns.addAll(productColumn, regionColumn, priceColumn, profitColumn, sentColumn, soldColumn)

    val buffer = new ObservableBuffer[Row]()
    buffer.addAll(supply.asJava)

    table.items = buffer

    table
  }
}
