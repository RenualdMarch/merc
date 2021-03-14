package mr.merc.ui.world

import javafx.scene.control.TableCell
import mr.merc.local.Localization
import mr.merc.politics.State
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.{ProgressBar, TableColumn, TableView}
import scalafx.scene.layout.BorderPane
import scalafx.Includes._
import scalafx.collections.ObservableBuffer

class TechnologyPane(states: List[State]) extends BorderPane {

  private val tableView = new TableView[State]()
  tableView.style = Components.largeFontStyle

  center = tableView

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

  val levelColumn = new StringColumn[State](Localization("technology.technologyLevel"),
    _.technologyLevel.technologyLevel.toString)

  val pointsInCurrentColumn = new StringColumn[State](Localization("technology.currentPoints"), x =>
    DoubleFormatter().format(x.technologyLevel.pointsInThisLevel))

  val pointsTillNextColumn = new StringColumn[State](Localization("technology.pointsRemained"), x =>
    IntFormatter().format(x.technologyLevel.pointsForNextLevel))

  private val progressColumn = new TableColumn[State, ProgressBar]() {
    text = Localization("project.progress")
    cellFactory = p => new TableCell[State, ProgressBar] {
      override def updateItem(t: ProgressBar, b: Boolean): Unit = {
        super.updateItem(t, b)
        Option(t).foreach { t =>
          t.prefWidth <== this.widthProperty()
        }
        setGraphic(t)
      }
    }
    cellValueFactory = p => ObjectProperty[ProgressBar]{
      new ProgressBar() {
        val tech = p.value.technologyLevel
        progress = tech.pointsInThisLevel / (tech.pointsInThisLevel + tech.pointsForNextLevel)
      }
    }
    editable = false
    prefWidth <== TechnologyPane.this.tableView.width * 0.2
  }

  tableView.columns ++= List(stateColumn, levelColumn, progressColumn, pointsInCurrentColumn, pointsTillNextColumn)

  tableView.items = ObservableBuffer[State]() ++ states
}