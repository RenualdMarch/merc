package mr.merc.ui.world

import javafx.scene.control.TableCell
import mr.merc.economics.EconomicRegion.ProductionTradingInfo
import mr.merc.local.Localization
import mr.merc.politics.State
import mr.merc.util.FxPropertyUtils
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control.{ScrollPane, SelectionMode, TableColumn, TableView}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer

class StateTradePane(stateProduction:Map[State, Map[State, ProductionTradingInfo]], playerState:State) extends PaneWithTwoHorizontalChildren(0.2) {
  private val list = new StatesListPane(stateProduction, playerState)
  private val right = new StateTradeRelations(stateProduction, list.selectedItem)

  setTwoChildren(list, right)
}

class StatesListPane(stateProduction:Map[State, Map[State, ProductionTradingInfo]], playerState:State) extends MigPane with WorldInterfaceJavaNode {
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

  statesTable.columns ++= List(stateColumn)

  private val buffer = new ObservableBuffer[State]()
  private val states = stateProduction.keySet.toList
  private val otherStates = playerState :: states.filterNot(_ == playerState)
  statesTable.items = buffer ++ otherStates

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.Single)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[State] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StateTradeRelations(stateProduction:Map[State, Map[State, ProductionTradingInfo]], currentStateProp:ReadOnlyObjectProperty[State]) extends PaneWithTwoHorizontalChildren {

  val list = new PropertyDependentPane[State](currentStateProp, sc => new SelectedStatesListPane(stateProduction, sc)) {
    lazy val selectedItem:ObjectProperty[State] = new ObjectProperty()

    override def reload(): Unit = {
      super.reload()
      val opt = Option(center.value).flatMap(x => Option(x.asInstanceOf[SelectedStatesListPane].selectedItem))
      opt.foreach { x =>
        selectedItem.value = x.value

        x.onChange {
          selectedItem.value = x.value
        }
      }
    }
  }

  val prop = FxPropertyUtils.mergeTwoProperties(currentStateProp, list.selectedItem)
  val statesPane = new PropertyDependentPane[(State, State)](prop, { case (currentState, selectedState) =>
    new StatesTradeBalance(stateProduction, currentState, selectedState)
  })

  setTwoChildren(list, statesPane)

}

class SelectedStatesListPane(stateProduction:Map[State, Map[State, ProductionTradingInfo]], selectedState: State) extends MigPane with WorldInterfaceJavaNode {
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

  private val exportColumn = new TableColumn[State, String] {
    text = Localization("trade.export")
    cellValueFactory = p => {
      val export = stateProduction(selectedState).getOrElse(p.value, ProductionTradingInfo(Map()))
      StringProperty(DoubleFormatter().format(export.totalSum))
    }
    editable = false
  }

  private val importColumn = new TableColumn[State, String] {
    text = Localization("trade.import")
    cellValueFactory = p => {
      val imp = stateProduction(p.value).getOrElse(selectedState, ProductionTradingInfo(Map()))
      StringProperty(DoubleFormatter().format(imp.totalSum))
    }
  }

  val balanceColumn = new TableColumn[State, String] {
    text = Localization("trade.balance")
    cellValueFactory = p => {
      val imp = stateProduction(p.value).getOrElse(selectedState, ProductionTradingInfo(Map()))
      val export = stateProduction(selectedState).getOrElse(p.value, ProductionTradingInfo(Map()))
      StringProperty(DoubleFormatter().format(export.totalSum - imp.totalSum))
    }
  }

  statesTable.columns ++= List(stateColumn, exportColumn, importColumn, balanceColumn)

  private val otherStates = stateProduction.keySet.filterNot(_ == selectedState)
  private val buffer = new ObservableBuffer[State]()
  statesTable.items = buffer ++ otherStates

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.Single)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[State] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StatesTradeBalance(stateProduction:Map[State, Map[State, ProductionTradingInfo]],
                         currentState: State, selectedState: State)
  extends ScrollPane {

  fitToWidth = true
  style = Components.largeFontStyle

  content = new MigPane {
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