package mr.merc.ui.world

import cats.kernel.Monoid
import javafx.scene.control.TableCell
import mr.merc.economics.EconomicRegion.ProductionTradingInfo
import mr.merc.local.Localization
import mr.merc.politics.State
import mr.merc.util.FxPropertyUtils
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control.{ScrollPane, SelectionMode, Tab, TabPane, TableColumn, TableView}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.control.TabPane.TabClosingPolicy

class StateTradePane(stateProduction: Map[State, Map[State, ProductionTradingInfo]], playerState: State) extends PaneWithTwoHorizontalChildren(0.33) {
  private val list = new StatesListPane(stateProduction, playerState)
  private val right = new SelectedStateTabPane(stateProduction, list.selectedItem)

  setTwoChildren(list, right)
}

class StatesListPane(stateProduction: Map[State, Map[State, ProductionTradingInfo]], playerState: State) extends MigPane with WorldInterfaceJavaNode {
  private val statesTable = new TableView[State]()
  statesTable.style = Components.mediumFontStyle

  val stateColumn = new TableColumn[State, StateComponentColorName] {

    text = Localization("diplomacy.state")

    cellFactory = p => new TableCell[State, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty(new StateComponentColorName(p.value))
    }
    editable = false
  }

  val exportColumn = new StringColumn[State](
    Localization("trade.export"),
    { p =>
      val sum = Monoid.combineAll(stateProduction(p).values).totalSum
      DoubleFormatter().format(sum)
    }
  )

  val importColumn = new StringColumn[State](
    Localization("trade.import"),
    { p =>
      val productions = stateProduction.map { case (_, sp) =>
        sp.getOrElse(p, ProductionTradingInfo(Map()))
      }

      val sum = Monoid.combineAll(productions).totalSum
      DoubleFormatter().format(sum)
    }
  )

  statesTable.columns ++= List(stateColumn, exportColumn, importColumn)

  private val buffer = new ObservableBuffer[State]()
  private val states = stateProduction.keySet.toList
  private val otherStates = playerState :: states.filterNot(_ == playerState)
  statesTable.items = buffer ++ otherStates

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.Single)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[State] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StateTradeRelations(stateProduction: Map[State, Map[State, ProductionTradingInfo]], currentState: State) extends PaneWithTwoHorizontalChildren {
  val list = new SelectedStateImportExportList(stateProduction, currentState)

  val statesPane = new PropertyDependentPane[State](list.selectedItem, { selectedState =>
    new StatesTradeBalance(stateProduction, currentState, selectedState)
  })

  setTwoChildren(list, statesPane)
}

class SelectedStateTabPane(stateProduction: Map[State, Map[State, ProductionTradingInfo]], selectedStateProperty: ReadOnlyObjectProperty[State]) extends MigPane with WorldInterfaceJavaNode {
  import FxPropertyUtils._

  private val tabPane = new TabPane {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  val exportTable = new Tab {
    text = Localization("trade.table")
    style = Components.largeFontStyle
    content <== selectedStateProperty.map(sc =>
      new SelectedStateImportExportTable(stateProduction, sc).delegate)
  }

  val exportList = new Tab {
    text = Localization("trade.list")
    style = Components.largeFontStyle
    content <== selectedStateProperty.map(sc =>
      new StateTradeRelations(stateProduction, sc).delegate
    )
  }

  tabPane.tabs.addAll(exportTable, exportList)
  add(tabPane, "grow, push")
}

class SelectedStateImportExportTable(stateProduction: Map[State, Map[State, ProductionTradingInfo]], selectedState: State) extends ScrollPane {
  fitToWidth = true
  style = Components.largeFontStyle

  content = new MigPane with WorldInterfaceWhiteJavaNode {
    val export = Monoid.combineAll(stateProduction(selectedState).values)
    val imp = Monoid.combineAll(stateProduction.values.map(m => m.getOrElse(selectedState, ProductionTradingInfo(Map()))))

    add(BigText(Localization("trade.export")), "wrap, center")
    val exportGrid = GridPaneBuilder.buildWithCaptionString(
      List(40d, 30d, 30d),
      export.perProduct.values.flatMap { pti =>
        List(EconomicLocalization.localizeProduct(pti.product),
          DoubleFormatter().format(pti.count), DoubleFormatter().format(pti.totalPrice))
      }.toList,
      List(Localization("enterprise.product"), Localization("enterprise.itemsSold"), Localization("enterprise.earnings")))
    add(exportGrid, "wrap, center, grow, span")
    add(BigText(Localization("trade.import")), "wrap, center")
    val importGrid = GridPaneBuilder.buildWithCaptionString(
      List(40d, 30d, 30d),
      imp.perProduct.values.flatMap { pti =>
        List(EconomicLocalization.localizeProduct(pti.product),
          DoubleFormatter().format(pti.count), DoubleFormatter().format(pti.totalPrice))
      }.toList,
      List(Localization("enterprise.product"), Localization("enterprise.itemsSold"), Localization("enterprise.earnings")))
    add(importGrid, "wrap, center, grow, span")
  }
}

class SelectedStateImportExportList(stateProduction: Map[State, Map[State, ProductionTradingInfo]], selectedState: State) extends MigPane with WorldInterfaceJavaNode {
  private val statesTable = new TableView[State]()
  statesTable.style = Components.mediumFontStyle

  private val stateColumn = new TableColumn[State, StateComponentColorName] {

    text = Localization("diplomacy.state")

    cellFactory = p => new TableCell[State, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty(new StateComponentColorName(p.value))
    }
    editable = false
  }

  private val exportColumn = new StringColumn[State](
    Localization("trade.export"),
    { p =>
      val export = stateProduction(selectedState).getOrElse(p, ProductionTradingInfo(Map()))
      DoubleFormatter().format(export.totalSum)
    }
  )

  private val importColumn = new StringColumn[State](
    Localization("trade.import"),
    { p =>
      val imp = stateProduction(p).getOrElse(selectedState, ProductionTradingInfo(Map()))
      DoubleFormatter().format(imp.totalSum)
    }
  )

  val balanceColumn = new StringColumn[State](
    Localization("trade.balance"),
    { p =>
      val imp = stateProduction(p).getOrElse(selectedState, ProductionTradingInfo(Map()))
      val export = stateProduction(selectedState).getOrElse(p, ProductionTradingInfo(Map()))
      DoubleFormatter().format(export.totalSum - imp.totalSum)
    }
  )

  statesTable.columns ++= List(stateColumn, balanceColumn, exportColumn, importColumn)

  private val otherStates = stateProduction.keySet.filterNot(_ == selectedState)
  private val buffer = new ObservableBuffer[State]()
  statesTable.items = buffer ++ otherStates

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.Single)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[State] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StatesTradeBalance(stateProduction: Map[State, Map[State, ProductionTradingInfo]],
                         currentState: State, selectedState: State)
  extends ScrollPane {

  fitToWidth = true
  style = Components.largeFontStyle

  content = new MigPane with WorldInterfaceWhiteJavaNode {
    val export = stateProduction(currentState).getOrElse(selectedState, ProductionTradingInfo(Map()))
    val imp = stateProduction(selectedState).getOrElse(currentState, ProductionTradingInfo(Map()))

    add(BigText(Localization("trade.export")), "wrap, center")
    val exportGrid = GridPaneBuilder.buildWithCaptionString(
      List(40d, 30d, 30d),
      export.perProduct.values.flatMap { pti =>
        List(EconomicLocalization.localizeProduct(pti.product),
          DoubleFormatter().format(pti.count), DoubleFormatter().format(pti.totalPrice))
      }.toList,
      List(Localization("enterprise.product"), Localization("enterprise.itemsSold"), Localization("enterprise.earnings")))
    add(exportGrid, "wrap, center, grow, span")
    add(BigText(Localization("trade.import")), "wrap, center")
    val importGrid = GridPaneBuilder.buildWithCaptionString(
      List(40d, 30d, 30d),
      imp.perProduct.values.flatMap { pti =>
        List(EconomicLocalization.localizeProduct(pti.product),
          DoubleFormatter().format(pti.count), DoubleFormatter().format(pti.totalPrice))
      }.toList,
      List(Localization("enterprise.product"), Localization("enterprise.itemsSold"), Localization("enterprise.earnings")))
    add(importGrid, "wrap, center, grow, span")
  }
}