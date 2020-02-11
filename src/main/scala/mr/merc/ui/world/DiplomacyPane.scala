package mr.merc.ui.world

import mr.merc.economics.{Culture, WorldStateDiplomacyActions}
import mr.merc.politics.{Province, State}
import mr.merc.util.MercTooltip
import org.tbee.javafx.scene.layout.MigPane
import javafx.scene.control.{SelectionMode, TableCell}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, TruceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.local.Localization
import mr.merc.ui.dialog.ModalDialog
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.Includes._
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.stage.Stage

import scala.collection.JavaConverters._
import mr.merc.util.FxPropertyUtils.PropertyBindingMap
import scalafx.scene.Node

import scala.util.Try

class DiplomacyPane(actions:WorldStateDiplomacyActions, currentState:State, stage: Stage) extends PaneWithTwoHorizontalChildren(0.2) {

  private val tablePane = new StatesTablePane(actions, currentState)
  private val property = tablePane.selectedItem
  private val selectedPane = new BorderPane {
    center <== property.map(Option.apply).map{
      case Some(p) => new StateDiplomacyPane(currentState, p, actions, stage)
      case None => new javafx.scene.layout.Pane()
    }
  }

  setTwoChildren(tablePane, selectedPane)
}

class StatesTablePane(actions: WorldStateDiplomacyActions, currentState: State) extends MigPane with WorldInterfaceJavaNode {
  private val statesTable = new TableView[State]()

  statesTable.width.onChange { (_, _, _) =>
    val header = lookup("TableHeaderRow").asInstanceOf[javafx.scene.layout.Pane]
    header.setMinHeight(0)
    header.setPrefHeight(0)
    header.setMaxHeight(0)
    header.setVisible(false)
  }

  statesTable.style = Components.largeFontStyle

  private val stateRectColumn = new TableColumn[State, Rectangle] {
    cellFactory = p => new TableCell[State, Rectangle] {
      override def updateItem(t: Rectangle, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      val rect = Rectangle(Components.largeFontSize, Components.largeFontSize)
      rect.fill = p.value.color
      rect.stroke = Color.Black
      ObjectProperty(rect)
    }
    editable = false
  }

  private val stateNameColumn = new TableColumn[State, String] {
    cellValueFactory = p => StringProperty(p.value.name)
    editable = false
  }

  private val relationsColumn = new TableColumn[State, String] {
    cellValueFactory = p => StringProperty(actions.relationships(currentState)(p.value).toString)
    editable = false
  }

  statesTable.columns ++= List(stateRectColumn, stateNameColumn, relationsColumn)

  private val buffer = new ObservableBuffer[State]()
  buffer.addAll((currentState :: actions.relationships(currentState).toList.sortBy(a => -Math.abs(a._2)).map(_._1)).asJava)
  statesTable.items = buffer

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[State] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StateDiplomacyPane(currentState:State, selectedState:State, actions: WorldStateDiplomacyActions, stage: Stage) extends MigPane {
  private val relations = new StateRelationsPane(currentState, selectedState, actions)
  private val agreements = new StateAgreementsAndClaimsPane(selectedState, actions)
  private val stateActions = if (currentState == selectedState) new Pane().delegate
  else new StateActionsPane(currentState, selectedState, actions, stage)


  add(relations, "grow")
  add(stateActions, "wrap")
  add(agreements, "span 2")

}

class StateRelationsPane(currentState:State, selectedState:State, actions: WorldStateDiplomacyActions) extends MigPane() {
  add(BigText(selectedState.name), "wrap")
  if (selectedState != currentState) {
    add(BigText(Localization("diplomacy.relationsToUs")))
    add(BigText(actions.relationships(selectedState)(currentState).toString), "wrap")
    actions.relationshipsDescribed(selectedState).getOrElse(currentState, Nil).foreach { b =>
      add(MediumText(b.title))
      add(MediumText(b.bonus.toString), "wrap")
    }
  }
}

class StateAgreementsAndClaimsPane(selectedState:State, actions: WorldStateDiplomacyActions) extends MigPane {

  private val agreements = actions.agreements(selectedState)
  private val vassalAgreements = agreements.collect { case v:VassalAgreement if v.vassal == selectedState => v}
  private val overlordAgreements = agreements.collect{ case v:VassalAgreement if v.overlord == selectedState => v}
  private val allianceAgreements = agreements.collect {case aa:AllianceAgreement => aa}
  private val wars = agreements.collect {case w:WarAgreement => w}
  private val truces = agreements.collect {case t:TruceAgreement => t}
  private val claims = actions.notFulfilledClaims(selectedState)

  private def addWar(war:WarAgreement): Unit = {
    add(MediumText(war.localizeWarName))
    add(new MigPane(){
      add(new StateComponentList(war.attackers.toList))
      add(MediumText(Localization("diplomacy.vs")))
      add(new StateComponentList(war.defenders.toList))
    }, "wrap")
  }

  if (vassalAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.vassalOf")))
    add(new StateComponentList(vassalAgreements.map(_.overlord)), "wrap")
  }

  if (overlordAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.overlordFor")))
    add(new StateComponentList(overlordAgreements.map(_.vassal)), "wrap")
  }

  if (allianceAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.alliances")))
    val alliances = allianceAgreements.flatMap(_.sides - selectedState)
    add(new StateComponentList(alliances), "wrap")
  }

  if (truces.nonEmpty) {
    add(MediumText(Localization("diplomacy.truces")))
    val currentTruces = truces.flatMap(_.sides - selectedState)
    add(new StateComponentList(currentTruces), "wrap")
  }

  if (claims.nonEmpty) {
    add(MediumText(Localization("diplomacy.claims")))
    add(new StateComponentList(actions.notFulfilledClaims(selectedState).map(_.targetState).distinct), "wrap")
  }

  if (wars.nonEmpty) {
    add(MediumText(Localization("diplomacy.wars")), "span 2, wrap")
    wars.foreach(addWar)
  }

}

class StateActionsPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions, stage:Stage) extends MigPane {

  import ModalDialog._

  private val declareWar = new MediumButton() {
    text = Localization("diplomacy.declareWar")
    onAction = {_ =>
      val dialog = new DeclareWarPane(currentState, selectedState, actions).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message)
      }
    }
  }

  private val proposePeace = new MediumButton() {
    text = Localization("diplomacy.proposePeace")
    onAction = {_ =>
      val dialog = new ProposePeacePane(currentState, selectedState, actions).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message)
      }
    }
  }

  private val proposeVassalization = new MediumButton() {
    text = Localization("diplomacy.proposeVassalization")
    onAction = {_ =>
      actions.sendMessage(new VassalizationProposal(currentState, selectedState))
    }
  }

  private val proposeOverlordship = new MediumButton() {
    text = Localization("diplomacy.proposeOverlordship")
    onAction = {_ =>
      actions.sendMessage(new OverlordshipProposal(currentState, selectedState))
    }
  }

  private val proposeAlliance = new MediumButton() {
    text = Localization("diplomacy.proposeAlliance")
    onAction = {_ =>
      actions.sendMessage(new AllianceProposal(currentState, selectedState))
    }
  }

  if (actions.canProposePeace(currentState, selectedState)) {
    add(proposePeace, "wrap")
  }

  if (actions.canDeclareWar(currentState, selectedState)) {
    add(declareWar, "wrap")
  }

  if (actions.canProposeAlliance(currentState, selectedState)) {
    add(proposeAlliance, "wrap")
  }

  if (actions.canProposeVassalization(currentState, selectedState)) {
    add(proposeVassalization, "wrap")
  }

  if (actions.canProposeOverlordship(currentState, selectedState)) {
    add(proposeOverlordship, "wrap")
  }
}

class DeclareWarPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends DialogStage[DeclareWar] {
  private val selectWarTarget = new SelectWarTarget(currentState, selectedState, Set(), actions)

  selectWarTarget.selectedWarTarget.onChange {
    dialogResult = selectWarTarget.selectedWarTarget.value.map { wt =>
      new DeclareWar(currentState, selectedState, wt, Set())
    }
  }

  override protected def dialogContent: Node = new MigPane {
    add(BigText(Localization("diplomacy.declareWarOn", selectedState.name)), "wrap")
    add(selectWarTarget)
  }

  override protected def css: Option[String] = None
}

class SelectWarTarget(currentState: State, selectedState: State, alreadyExistedTargets:Set[WarTarget], actions: WorldStateDiplomacyActions) extends PaneWithTwoHorizontalChildren {
  private abstract class WarTargetSelection extends BorderPane {
    def selectedWarTarget:Option[WarTarget]
  }

  private trait WarTargetKind {
    def component:Option[WarTargetSelection]

    def label:String
  }

  private class VassalizeTargetKind extends WarTargetKind {
    private val possibleVassalizationTargets = actions.possibleVassalizationWarTargets(selectedState).toSet --
      alreadyExistedTargets.collect { case v:Vassalize => v.giver }

    override def component: Option[WarTargetSelection] = {
      val child = new WarTargetSelection {
        val tg = new ToggleGroup()
        center = new MigPane {
          possibleVassalizationTargets.foreach { pt =>
            val rb = new RadioButton {
              text = pt.name
              toggleGroup = tg
              userData = pt
            }
            add(rb, "wrap")
          }
        }

        override def selectedWarTarget: Option[WarTarget] = tg.selectedToggle.map{ state =>
          Option(state).map { st =>
            new Vassalize(currentState, st.userData.asInstanceOf[State])
          }
        }.value
      }

      if (possibleVassalizationTargets.isEmpty) None else Some(child)
    }

    override def label: String = Localization("diplomacy.vassalize")
  }

  private class DemandMoneyTargetKind extends WarTargetKind {
    override def component: Option[WarTargetSelection] = Some(new WarTargetSelection {
      override def selectedWarTarget: Option[WarTarget] = Try(textField.text.value.toDouble).map {d =>
        new TakeMoney(currentState, selectedState, d)
      }.toOption

      private val textField = new TextField {
        text = "0"
      }
      center = new MigPane {
        add(MediumText(Localization("diplomacy.demand.money.from", selectedState)), "wrap")
        add(textField)
      }
    })

    override def label: String = Localization("diplomacy.demand.money")
  }

  private class TakeProvinceTargetKind extends WarTargetKind {
    private val possibleProvinces = actions.possibleProvincesToTake(selectedState).toSet --
      alreadyExistedTargets.collect { case t:TakeProvince => t.province }

    override def component: Option[WarTargetSelection] = if (possibleProvinces.nonEmpty) {
      Some(new WarTargetSelection {
        private val group = new ToggleGroup()

        override def selectedWarTarget: Option[WarTarget] = group.selectedToggle.map(Option.apply).map {
          case Some(toggle) =>
            val province = toggle.getUserData.asInstanceOf[Province]
            Some(new TakeProvince(currentState,selectedState, province))
          case None => None
        }.value

        center = new MigPane {
          possibleProvinces.foreach { p =>
            val radio = new RadioButton {
              text = p.name
              toggleGroup = group
              userData = p
            }
            add(radio, "wrap")
          }
        }
      })
    } else None

    override def label: String = Localization("diplomacy.takeProvince")
  }

  private class CollapseTargetKind extends WarTargetKind {
    private val possibleStates = Set(selectedState) -- alreadyExistedTargets.collect {
      case c:CrackState => c.giver
    }

    override def component: Option[WarTargetSelection] = if (possibleStates.nonEmpty) {
      Some(
        new WarTargetSelection {
          private val group = new ToggleGroup()

          override def selectedWarTarget: Option[WarTarget] = group.selectedToggle.map(Option.apply).map {
            case Some(toggle) =>
              val state = toggle.getUserData.asInstanceOf[State]
              Some(new CrackState(currentState, state))
            case None => None
          }.value

          center = new MigPane {
            possibleStates.foreach { s =>
              val radio = new RadioButton {
                text = s.name
                toggleGroup = group
                userData = s
              }
              add(radio, "wrap")
            }
          }
        }
      )} else None

    override def label: String = Localization("diplomacy.collapseState")
  }

  private class LiberateCultureTargetKind extends WarTargetKind {
    private val possibleCultures = actions.possibleCulturesToLiberate(selectedState) -- alreadyExistedTargets.collect {
      case lc:LiberateCulture => lc.culture
    }

    override def component: Option[WarTargetSelection] = if (possibleCultures.nonEmpty) {
      Some(
        new WarTargetSelection {
          private val group = new ToggleGroup()

          override def selectedWarTarget: Option[WarTarget] = group.selectedToggle.map(Option.apply).map {
            case Some(toggle) =>
              val culture = toggle.getUserData.asInstanceOf[Culture]
              Some(new LiberateCulture(currentState, selectedState, culture,
                actions.provincesByCulture(selectedState, culture).toSet))
            case None => None
          }.value

          center = new MigPane {
            possibleCultures.foreach { s =>
              val radio = new RadioButton {
                text = s.name
                toggleGroup = group
                userData = s
              }
              add(radio, "wrap")
            }
          }
        }
      )} else None

    override def label: String = Localization("diplomacy.liberateCulture")
  }

  private def possibleWarTargetKinds:List[WarTargetKind] = {
    List(new VassalizeTargetKind,
      new DemandMoneyTargetKind,
      new TakeProvinceTargetKind,
      new CollapseTargetKind,
      new LiberateCultureTargetKind).filter(_.component.nonEmpty)
  }

  private class RadioTargetKindsPane(kinds:List[WarTargetKind]) extends MigPane {
    private val group = new ToggleGroup()

    kinds.foreach { k =>
      val btn = new RadioButton {
        text = k.label
        toggleGroup = group
        userData = k

      }
      add(btn, "wrap")
    }

    val selectedWarTargetKind:ObjectProperty[WarTargetKind] = group.selectedToggle.map {t =>
      t.userData.asInstanceOf[WarTargetKind]
    }
  }

  private val left = new RadioTargetKindsPane(possibleWarTargetKinds)
  private val property = left.selectedWarTargetKind
  private val right = new BorderPane {
    val centerComponent = property.map(Option.apply).map(_.flatMap(_.component))
    center <== centerComponent.map {
      case Some(c) => c.delegate
      case None => new Pane().delegate
    }
  }

  setTwoChildren(left, right)

  val selectedWarTarget:ObjectProperty[Option[WarTarget]] = right.centerComponent.map(_.flatMap(_.selectedWarTarget))
}

class ProposePeacePane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends DialogStage[ProposePeace] {

  // TODO case when two simultaneous wars, however it is very unlikely
  private val warOpt = actions.warsForWhichCanProposePeace(currentState, selectedState).headOption

  def targets:List[WarTarget] = {
    warOpt.map { war =>
      val currentSide = if (war.attackers.contains(currentState)) war.attackers
      else if (war.defenders.contains(currentState)) war.defenders
      else sys.error(s"Impossible propose peace for $currentState in war $war")

      war.targets.filter { target =>
        currentSide.contains(target.demander)
      }.toList
    }.getOrElse(Nil)
  }

  private var selectedTargets:Set[WarTarget] = Set()

  private val checkButtons = targets.map { t =>
    new CheckBox(t.localizeTarget) {
      onAction = {_ =>
        if (this.selected.value) selectedTargets += t
        else selectedTargets -= t
        warOpt.foreach { war =>
          dialogResult = Some(new ProposePeace(currentState, selectedState, war, selectedTargets))
        }
      }
    }
  }

  override val dialogContent:Node = new MigPane {
    checkButtons.foreach { cb =>
      add(cb, "wrap")
    }
  }

  override val css:Option[String] = None

}

class StateComponentColorName(state: State) extends MigPane("center") with WorldInterfaceJavaNode {
  val rect = Rectangle(Components.mediumFontSize, Components.mediumFontSize)
  rect.fill = state.color
  rect.stroke = Color.Black
  val text = MediumText(state.name)
  add(rect)
  add(text)
}

class StateComponentList(list: List[State]) extends MigPane {

  private def buildStateColor(s:State):StateComponentColor = new StateComponentColor(s)
  private def buildStateColorName(s:State):StateComponentColorName = new StateComponentColorName(s)

  private val f = if (list.size > 10) buildStateColor _
   else buildStateColorName _

  list.foreach { s =>
    add(f(s))
  }
}

class StateComponentColor(state: State) extends MigPane("center") {
  val rect = Rectangle(Components.mediumFontSize, Components.mediumFontSize)
  rect.fill = state.color
  rect.stroke = Color.Black
  add(rect)
  MercTooltip.applyTooltip(rect, state.name)
}