package mr.merc.ui.world

import javafx.scene.control.TableCell
import mr.merc.economics.WorldStateDiplomacyActions
import mr.merc.local.Localization
import mr.merc.politics.State
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.layout.BorderPane
import scalafx.Includes._
import scalafx.collections.ObservableBuffer

class RatingPane(actions: WorldStateDiplomacyActions, currentState: State) extends BorderPane with WorldInterfaceWhiteNode {
  val tableView = new TableView[State]()

  center = tableView

  tableView.style = Components.mediumFontStyle

  val stateNameColumn = new TableColumn[State, StateComponentColorName] {

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

  val provincesCountColumn = new StringColumn[State](Localization("regions"),
    s => actions.states(s).size.toString)

  val populationCountColumn = new StringColumn[State](Localization("populationCount"),
    s => IntFormatter().format(actions.states(s).map(_.totalPopulation).sum)
  )

  val armySizeCountColumn = new StringColumn[State](Localization("army"),
    s => actions.regions.flatMap(_.regionWarriors.allWarriors).count(_.owner == s).toString)

  tableView.columns ++= List(stateNameColumn, provincesCountColumn, armySizeCountColumn, populationCountColumn)

  val sortedStates = actions.states.toList.sortBy(-_._2.map(_.totalPopulation).sum).map(_._1)
  tableView.items = new ObservableBuffer[State]() ++ sortedStates
}
