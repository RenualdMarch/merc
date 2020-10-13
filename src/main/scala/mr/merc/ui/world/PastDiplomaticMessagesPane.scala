package mr.merc.ui.world

import mr.merc.diplomacy.DiplomaticMessage
import scalafx.scene.control.{RadioButton, ScrollPane, ToggleGroup}
import mr.merc.economics.WorldStateDiplomacyActions
import mr.merc.local.Localization
import mr.merc.ui.world.PastDiplomaticMessagesPane.{NeighboursAndAgreements, SelectedPastMessagesOption}
import org.tbee.javafx.scene.layout.MigPane
import mr.merc.util.FxPropertyUtils._
import scalafx.Includes._

class PastDiplomaticMessagesPane(actions:WorldStateDiplomacyActions) extends MigPane {

  val group = new ToggleGroup()
  val selectedOptionProperty = group.selectedToggle.map(
    _.getUserData.asInstanceOf[SelectedPastMessagesOption])

  val radioButtons = new MigPane {

    val buttons = PastDiplomaticMessagesPane.Possible.map { opt =>
      new RadioButton {
        text = opt.localizedMessage
        style = Components.largeFontStyle
        userData = opt
        selected = opt == NeighboursAndAgreements
        toggleGroup = group
      }
    }

    buttons.foreach { rb =>
      add(rb)
    }
  }

  val scrollPane = new ScrollPane {
    fitToWidth = true

    val migPane = new MigPane()

    def refreshMessages(): Unit = {
      val filtering = selectedOptionProperty.value
      migPane.children.clear()
      filtering.filter(actions, actions.diplomacyEngine.turnMessagesReport).flatMap(_.renderInReport).foreach {
        m => migPane.add(m, "wrap")
      }
    }

    selectedOptionProperty.onChange {
      refreshMessages()
    }

    refreshMessages()

    content = migPane
  }

  add(radioButtons, "wrap")
  add(scrollPane, "push, grow")

}

object PastDiplomaticMessagesPane {
  sealed trait SelectedPastMessagesOption {
    def localizedMessage: String
    def filter(actions: WorldStateDiplomacyActions, messages: List[DiplomaticMessage]): List[DiplomaticMessage]
  }
  object AllEvents extends SelectedPastMessagesOption {

    override def localizedMessage: String = Localization("messages.events.all")

    override def filter(actions: WorldStateDiplomacyActions, messages: List[DiplomaticMessage]): List[DiplomaticMessage] = messages
  }

  object NeighbourEvents extends SelectedPastMessagesOption {

    override def localizedMessage: String = Localization("messages.events.neighbour")

    override def filter(actions: WorldStateDiplomacyActions, messages: List[DiplomaticMessage]): List[DiplomaticMessage] = {
      val neighbours = actions.playerNeighbours
      messages.filter { message =>
        neighbours.contains(message.from) || neighbours.contains(message.to)
      }
    }
  }

  object NeighboursAndAgreements extends SelectedPastMessagesOption {

    override def localizedMessage: String = Localization("messages.events.neighboursAndAgreements")

    override def filter(actions: WorldStateDiplomacyActions, messages: List[DiplomaticMessage]): List[DiplomaticMessage] = {
      val agreements = actions.agreements(actions.playerState).to[Set].flatMap(_.sides)
      val neighbours = actions.playerNeighbours

      messages.filter { message =>
        neighbours.contains(message.from) || neighbours.contains(message.to) ||
          agreements.contains(message.from) || agreements.contains(message.to)
      }
    }
  }
  object OurEvents extends SelectedPastMessagesOption {

    override def localizedMessage: String = Localization("messages.events.our")

    override def filter(actions: WorldStateDiplomacyActions, messages: List[DiplomaticMessage]): List[DiplomaticMessage] = {
      messages.filter { message =>
        message.from == actions.playerState || message.to == actions.playerState
      }
    }
  }

  val Possible = List(AllEvents, NeighboursAndAgreements, NeighbourEvents, OurEvents)
}
