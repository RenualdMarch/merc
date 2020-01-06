package mr.merc.ui.world

import mr.merc.economics.WorldStateParliamentActions
import mr.merc.local.Localization._
import mr.merc.local.Localization
import mr.merc.politics._
import mr.merc.ui.common.SceneManager
import mr.merc.ui.dialog.ModalDialog
import mr.merc.ui.world.PieChartBuilder.PiePart
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.Includes._
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty}
import scalafx.geometry.{Orientation, Pos}
import scalafx.scene.shape.Circle
import mr.merc.util.FxPropertyUtils._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.{Separator, Tab, TabPane}
import scalafx.scene.paint.Color
import scalafx.stage.Stage
import ModalDialog._

class ParliamentPane(sceneManager: SceneManager, worldState: WorldStateParliamentActions) extends BorderPane {
  left = new RulingPartyParliamentPane(sceneManager, worldState)
  center = new ParliamentPie(worldState)
}

class RulingPartyParliamentPane(sceneManager: SceneManager, worldState: WorldStateParliamentActions) extends MigPane {

  private val rulingPartyPane = new TopTitledBorderPane {
    top = MediumText(Localization("parliament.rulingParty"))
    center = new PartyViewPane(worldState.playerPoliticalSystemProperty.map(_.rulingParty))
  }

  private val actionsPane = new ParliamentActionsPane(sceneManager, worldState)

  private val coalitionPaneProp:ReadOnlyObjectProperty[javafx.scene.Node] = worldState.playerPoliticalSystemProperty.map { ps =>
    ps.parliament.map { p =>
      val coalition = p.coalition.map(party => party -> p.parties(party)).toMap
      val pane: Pane = new ParliamentCoalitionPane(coalition)
      pane.delegate
    }.getOrElse(new Pane().delegate)
  }

  add(rulingPartyPane, "wrap")
  add(new BorderPane {
    center <== coalitionPaneProp
  }.delegate, "wrap,grow")
  add(actionsPane)
}

class ParliamentActionsPane(sceneManager: SceneManager, worldState: WorldStateParliamentActions) extends MigPane {
  private val changeRulingParty = MediumButton(Localization("parliament.changeRulingParty"))
  changeRulingParty.disable <== !worldState.playerCanChangeRulingParty
  changeRulingParty.onAction = { _ =>
    val partyOpt = choosePartiesDialog(worldState.possibleParties(worldState.playerPoliticalSystemProperty.value))
    partyOpt.foreach { p =>
      worldState.changeRulingParty(worldState.playerState, p)
    }
  }

  private val usurpPower = MediumButton(Localization("parliament.usurpPower"))
  usurpPower.disable <== !worldState.playerCanUsurpPower
  usurpPower.onAction = { _ =>
    val partyOpt = choosePartiesDialog(worldState.possiblePartiesForUsurpation(worldState.playerState))
    partyOpt.foreach { p =>
      worldState.usurpPower(worldState.playerState, p)
    }
  }

  private val giveUpPower = MediumButton(Localization("parliament.giveUpPower"))
  giveUpPower.disable <== !worldState.playerCanGiveUpPower
  giveUpPower.onAction = { _ =>
    val partyOpt = choosePartiesDialog(worldState.possiblePartiesForGivingUpPower(worldState.playerState))
    partyOpt.foreach { p =>
      worldState.giveUpPower(worldState.playerState, p)
    }
  }

  add(changeRulingParty, "grow,wrap")
  add(usurpPower, "grow,wrap")
  add(giveUpPower, "grow,wrap")

  def choosePartiesDialog(possibleParties: List[Party]): Option[Party] =
    new SelectPartyPane(possibleParties).showDialog(sceneManager.stage).selected

}

// TODO in future add tooltips
class PartyViewPane(party: ReadOnlyObjectProperty[Party]) extends BorderPane with WorldInterfaceWhiteNode {
  private val titlePane = {
    val pane = new MigPane("center")
    val square = new Circle()
    square.radius <== Components.largeFontSize / 2
    square.fill <== party.map(_.color.delegate)
    square.stroke = Color.Black
    pane.add(square)
    val partyName = BigText(party.map(p => Localization(p.name)))
    pane.add(partyName)
    pane
  }

  def this(party: Party) = this(ObjectProperty(party))

  top = titlePane
  BorderPane.setAlignment(titlePane, Pos.TopCenter)
  center = {
    val pane = new MigPane()
    pane.add(MediumText(Localization("parliament.regime")))
    pane.add(MediumText(party.map(p => Localization(p.regime.name))), "wrap")
    pane.add(MediumText(Localization("parliament.votersPolicy")))
    pane.add(MediumText(party.map(p => Localization(p.votersPolicy.name))), "wrap")
    pane.add(MediumText(Localization("parliament.foreignPolicy")))
    pane.add(MediumText(party.map(p => Localization(p.foreignPolicy.name))), "wrap")
    pane.add(MediumText(Localization("parliament.economy")))
    pane.add(MediumText(party.map(p => Localization(p.economy.name))), "wrap")
    pane.add(MediumText(Localization("parliament.migration")))
    pane.add(MediumText(party.map(p => Localization(p.migration.name))), "wrap")
    pane.add(MediumText(Localization("parliament.socialPolicy")))
    pane.add(MediumText(party.map(p => Localization(p.socialPolicy.name))), "wrap")
  }
}

class ParliamentPie(worldState: WorldStateParliamentActions) extends TabPane {
  style = Components.mediumFontStyle

  private def pieByVotes(votes: Map[Party, Double]): Node = {
    import mr.merc.economics.MapUtil.FloatOperations.MapWithFloatOperations

    val percentage = votes.scaleToSum(1d)
    val pies = votes.toList.sortBy(-_._2).map { case (p, count) =>
      val name = Localization(p.name)
      val tooltip = s"$name\n${DoubleFormatter().format(percentage(p) * 100)}%"
      PiePart(p.color, name, count, Some(tooltip))
    }
    PieChartBuilder.build(pies)
  }

  private val parliamentTab = new Tab {
    text = Localization("parliament.parliament")
    content <== worldState.playerPoliticalSystemProperty.map { ps =>
      ps.parliament.map(_.parties).map(pieByVotes).getOrElse {
        BigText(Localization("parliament.noParliament"))
      }.delegate
    }
  }

  private val votersPopularityTab = new Tab() {
    text = Localization("parliament.votersPopularity")
    content <== worldState.playerPoliticalSystemProperty.map { _ =>
      pieByVotes(worldState.partyPopularityAmongVoters(worldState.playerState)).delegate
    }
  }

  private val popularityTab = new Tab() {
    text = Localization("parliament.popularity")
    content <== worldState.playerPoliticalSystemProperty.map { _ =>
      pieByVotes(worldState.partyPopularity(worldState.playerState)).delegate
    }
  }

  this.tabs.addAll(parliamentTab, votersPopularityTab, popularityTab)
  this.tabClosingPolicy = TabClosingPolicy.Unavailable
}

class SelectPartyPane(possibleParties: List[Party]) extends Stage {
  var selected: Option[Party] = None

  class ParentPartyPane(val child: Party) extends BorderPane {
    center = new PartyViewPane(child)
    styleClass.add("party-pane")

    this.onMouseClicked = { _ =>
      this.requestFocus()
      selected = Some(child)
    }
  }

  this.scene = new Scene {
    stylesheets.add("/css/partyPane.css")
    content = {
      val pane = new Pane()
      val parentMigPane = new MigPane()
      val migPaneScala: Pane = parentMigPane
      val okButton = MediumButton(Localization("common.ok"))
      val cancelButton = MediumButton(Localization("common.cancel"))

      migPaneScala.prefWidth <== pane.width
      migPaneScala.prefHeight <== pane.height - okButton.height * 2

      pane.children.addAll(parentMigPane)

      cancelButton.onAction = { _ =>
        selected = None
        SelectPartyPane.this.close()
      }

      okButton.onAction = { _ =>
        SelectPartyPane.this.close()
      }

      val partiesPane = new MigPane("wrap 4")

      val partyPanes = possibleParties.map(new ParentPartyPane(_))
      partyPanes.foreach { pp =>
        partiesPane.add(pp, "grow")
      }
      partyPanes.head.requestFocus()

      val buttonsPane = new MigPane()
      buttonsPane.add(okButton)
      buttonsPane.add(cancelButton)

      parentMigPane.add(partiesPane, "wrap")
      parentMigPane.add(buttonsPane, "center")

      pane
    }
  }

  this.onCloseRequest = { _ =>
    this.selected = None
  }
}

class ParliamentCoalitionPane(parties: Map[Party, Double]) extends MigPane() {
  private val sorted = parties.toList.sortBy(-_._2)

  add(BigText(Localization("parliament.coalition")), "center,grow,push,span 2,wrap")
  add(new Separator {
    style = s"-fx-border-width: ${Components.mediumFontSize / 10} 0 0 0; -fx-border-style: solid;"
    orientation = Orientation.Horizontal
  }.delegate, "grow,push,wrap,span 2")
  sorted.foreach { case (p, c) =>
    add(MediumText(p.name.localize))
    add(MediumText(DoubleFormatter().format(100 * c) + "%"), "wrap")
  }

}