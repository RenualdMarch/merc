package mr.merc.ui.world

import javafx.scene.control.SelectionMode
import mr.merc.diplomacy.DiplomaticMessage.DeclareWar
import mr.merc.diplomacy.{DiplomaticDeclaration, DiplomaticMessage, DiplomaticProposal}
import mr.merc.economics.WorldStateDiplomacyActions
import mr.merc.local.Localization
import mr.merc.politics.State
import org.tbee.javafx.scene.layout.MigPane
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.control.{CheckBox, TableColumn, TableView, TextArea}
import scalafx.Includes._
import scalafx.scene.layout.{BorderPane, Pane}
import mr.merc.util.FxPropertyUtils._
import scalafx.collections.ObservableBuffer
import scalafx.scene.Node
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

class MailPane(playerState:State, actions: WorldStateDiplomacyActions) extends PaneWithTwoHorizontalChildren(0.4) {
  val titles = new MailTitles(playerState, actions)
  val mail = new MailView(actions, titles.selectedMessage, () => titles.refresh())
  setTwoChildren(titles, mail)
}

class MailTitles(playerState:State, actions: WorldStateDiplomacyActions) extends BorderPane {

  private val tableView = new TableView[DiplomaticMessage]()
  tableView.style = Components.largeFontStyle

  center = tableView

  private val senderColumn = new TableColumn[DiplomaticMessage, StateComponentColorName] {
    cellFactory = p => new javafx.scene.control.TableCell[DiplomaticMessage, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => ObjectProperty(new StateComponentColorName(p.value.from))
    editable = false
  }

  private val titleColumn = new TableColumn[DiplomaticMessage, String] {
    cellValueFactory = p => StringProperty(p.value.messageTitle)
    editable = false
  }

  tableView.columns ++= List(senderColumn, titleColumn)

  refresh()
  tableView.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  tableView.delegate.getSelectionModel.clearAndSelect(0)

  def selectedMessage: ReadOnlyObjectProperty[DiplomaticMessage] =
    tableView.delegate.getSelectionModel.selectedItemProperty


  def refresh(): Unit = {
    tableView.items = ObservableBuffer() ++ actions.mailbox(playerState)
  }
}

class MailView(actions: WorldStateDiplomacyActions, selectedMessage: ReadOnlyObjectProperty[DiplomaticMessage], refreshTable: () => Unit) extends BorderPane {

   val prop = selectedMessage.map(Option.apply).map {
    case None => new Pane()
    case Some(x) => x match {
      case d: DiplomaticDeclaration => new DeclarationMessageView(actions, d, refreshTable)
      case q: DiplomaticProposal => new QuestionMessageView(actions, q, refreshTable)
      case dw: DeclareWar => new DeclareWarMessageView(actions, dw, refreshTable)
    }
  }.map(_.delegate)

  center <== prop
}

class MessageViewParent(message:DiplomaticMessage) extends BorderPane {
  private val title = BigText(message.messageTitle)
  private def buildStateRect() = new Rectangle {
    fill = message.from.color
    stroke = Color.Black
    width <== this.height
  }

  private val rectangle1 = buildStateRect()
  private val rectangle2 = buildStateRect()

  top = new MigPane {
    add(rectangle1, "center")
    add(title, "center")
    add(rectangle2, "center")
  }
}

class DeclarationMessageView(actions: WorldStateDiplomacyActions, message: DiplomaticDeclaration, refreshTable: () => Unit) extends MessageViewParent(message) {

  val textPane = new TextArea() {
    text = message.body
    style = Components.largeFontStyle
    editable = false
  }

  val okButton = new BigButton {
    text = Localization("diplomacy.acknowledge")
    onAction = {_ =>
      actions.acknowledgeMessage(message)
      refreshTable()
    }
  }

  center = textPane
  bottom = new MigPane {
    add(okButton, "center")
  }
}

class QuestionMessageView(actions: WorldStateDiplomacyActions, message: DiplomaticProposal, refreshTable: () => Unit) extends MessageViewParent(message) {
  val textPane = new TextArea() {
    text = message.body
    editable = false
    style = Components.largeFontStyle
  }

  val okButton = new BigButton {
    text = Localization("diplomacy.accept")
    onAction = {_ =>
      actions.answerMessage(message, true)
      refreshTable()
    }
  }

  val cancelButton = new BigButton {
    text = Localization("diplomacy.decline")
    onAction = {_ =>
      actions.answerMessage(message, false)
      refreshTable()
    }
  }

  center = textPane
  bottom = new MigPane {
    add(okButton, "center")
    add(cancelButton, "center")
  }
}

class DeclareWarMessageView(actions: WorldStateDiplomacyActions, message: DeclareWar, refreshTable: () => Unit) extends MessageViewParent(message) {
  private val allies = actions.allies(message.to)

  val textPane = new TextArea() {
    text = message.body
    editable = false
    style = Components.largeFontStyle
  }

  val alliesToCallMessage = BigText(Localization("diplomacy.alliesToCall"))

  private val selectedAllies:ObjectProperty[Set[State]] = ObjectProperty[Set[State]](Set[State]())

  val alliesPane = new MigPane {
    allies.foreach { a =>
      val checkBox = new CheckBox {
        selected = false
        text = a.name
        onAction = {_ =>
          if (this.selected.value) selectedAllies.value += a
          else selectedAllies.value -= a
        }
      }
      add(checkBox, "wrap")
    }
  }

  val okButton = new BigButton {
    text = Localization("diplomacy.okWar")
    onAction = { _ =>
      actions.answerDeclareWar(message, selectedAllies.value)
      refreshTable()
    }
  }

  val centerPane = new MigPane {
    add(textPane, "wrap")
    if (allies.nonEmpty) {
      add(alliesToCallMessage, "wrap")
      add(alliesPane)
    }
  }

  center = centerPane

  bottom = new MigPane {
    add(okButton, "center")
  }

}