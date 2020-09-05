package mr.merc.ui.world

import mr.merc.economics.Culture
import mr.merc.local.Localization
import mr.merc.politics.{Party, State, StateElectionReport}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, StringProperty}
import scalafx.scene.Node
import scalafx.scene.control.{TableColumn, TableView}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.Includes._
import scalafx.collections.ObservableBuffer

class ElectionResultsPane(electionResults:StateElectionReport, state:State) extends MigPane {

  add(BigText(Localization("election.results")), "center, wrap")
  add(electionResultsTable(electionResults.votes), "center, grow, wrap")
  state.politicalSystem.parliament.foreach { parliament =>
    add(BigText(Localization("menu.parliament")), "center, grow, wrap")
    add(ParliamentPie.pieByVotes(parliament.parties), "center, grow, push, wrap")
  }

  private def electionResultsTable(votes:Map[Party, Double]):Node = {
    val tableView = new TableView[(Party, Double)]()
    tableView.style = Components.largeFontStyle

    val votesSum = votes.values.sum

    val partyColumn = new TableColumn[(Party, Double), Node] {
      cellFactory = p => new javafx.scene.control.TableCell[(Party, Double), Node] {
        override def updateItem(t: Node, b: Boolean): Unit = {
          super.updateItem(t, b)
          setGraphic(t)
        }
      }

      cellValueFactory = { p =>
        val (party, _) = p.value
        val node: Node = new PartyComponentColorName(party)
        ObjectProperty(node)
      }

      editable = false
    }

    val votesColumn = new TableColumn[(Party, Double), String] {
      style = "-fx-alignment: CENTER-RIGHT;"
      cellValueFactory = { p =>
        val (_, votes) = p.value
        StringProperty(Math.ceil(votes).toLong.toString)
      }

      editable = false
    }

    val percentageColumn = new TableColumn[(Party, Double), String] {
      style = "-fx-alignment: CENTER-RIGHT;"
      cellValueFactory = { p =>
        val (_, votes) = p.value
        StringProperty(Math.ceil(votes*100/votesSum).toLong.toString + "%")
      }

      editable = false
    }

    tableView.columns ++= List(partyColumn, percentageColumn, votesColumn)
    tableView.items = ObservableBuffer() ++ votes.toList.sortBy(-_._2)
    tableView
  }
}

class PartyComponentColorName(party: Party) extends MigPane("") with WorldInterfaceJavaNode {
  val rect = Circle(Components.mediumFontSize)
  rect.fill = party.color
  rect.stroke = Color.Black
  val text = BigText(Localization(party.name))
  add(rect)
  add(text)
}

class CultureComponentColorName(culture: Culture) extends MigPane("") with WorldInterfaceJavaNode {
  val rect = Circle(Components.mediumFontSize)
  rect.fill = culture.color
  rect.stroke = Color.Black
  val text = BigText(EconomicLocalization.localizeCulture(culture))
  add(rect)
  add(text)
}