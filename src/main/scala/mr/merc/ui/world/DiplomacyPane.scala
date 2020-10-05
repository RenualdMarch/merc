package mr.merc.ui.world

import mr.merc.economics.{Culture, Seasons, WorldConstants, WorldStateDiplomacyActions}
import mr.merc.politics.{Province, State}
import mr.merc.util.MercTooltip
import org.tbee.javafx.scene.layout.MigPane
import javafx.scene.control.{SelectionMode, TableCell}
import mr.merc.diplomacy.Claim
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, TruceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.economics.WorldStateDiplomacyActions.StateInfo
import mr.merc.local.Localization
import mr.merc.ui.dialog.ModalDialog
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, Pane, Region}
import scalafx.Includes._
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.collections.ObservableBuffer
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.stage.Stage
import mr.merc.util.FxPropertyUtils.PropertyBindingMap
import scalafx.scene.{Node, Scene}
import scalafx.scene.control.TabPane.TabClosingPolicy

class DiplomacyPane(actions: WorldStateDiplomacyActions, currentState: State, stage: Stage, parentFrame:WorldFrame) extends PaneWithTwoHorizontalChildren(0.45) {

  private val tablePane = new StatesTablePane(actions, currentState)
  private val property = tablePane.selectedItem
  private val selectedPane = new BorderPane {
    center <== property.map(Option.apply).map {
      case Some(p) => new StateDiplomacyPane(currentState, p, actions, stage, parentFrame)
      case None => new javafx.scene.layout.Pane()
    }
  }

  setTwoChildren(tablePane, selectedPane)
}

class StatesTablePane(actions: WorldStateDiplomacyActions, currentState: State) extends MigPane with WorldInterfaceJavaNode {
  private val statesTable = new TableView[StateInfo]()

  /*statesTable.width.onChange { (_, _, _) =>
    val header = lookup("TableHeaderRow").asInstanceOf[javafx.scene.layout.Pane]
    header.setMinHeight(0)
    header.setPrefHeight(0)
    header.setMaxHeight(0)
    header.setVisible(false)
  }

  width.onChange { (_, _, _) =>
    val header = lookup("TableHeaderRow").delegate.asInstanceOf[javafx.scene.layout.Pane]
    header.setMinHeight(0)
    header.setPrefHeight(0)
    header.setMaxHeight(0)
    header.setVisible(false)
  }
   */

  statesTable.style = Components.mediumFontStyle

  val stateColumn = new TableColumn[StateInfo, StateComponentColorName] {

    text = Localization("diplomacy.state")

    cellFactory = p => new TableCell[StateInfo, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty(new StateComponentColorName(p.value.state))
    }
    editable = false
  }

  private val relationsColumn = new StringColumn[StateInfo](
    Localization("diplomacy.relationsToUs"),
    p => actions.relationships(p.state)(currentState).toString
  )

  private val armyColumn = new StringColumn[StateInfo](Localization("army"), _.army.toString)

  private val incomeColumn = new StringColumn[StateInfo](Localization("budget.income"),
    x => DoubleFormatter().format(x.income))

  private val spendingColumn = new StringColumn[StateInfo](Localization("budget.spending"),
    x => DoubleFormatter().format(x.spending))

  private val moneyReservesColumn = new StringColumn[StateInfo](Localization("budget.reserve"),
    x => DoubleFormatter().format(x.moneyReserve))

  private val literacyColumn = new StringColumn[StateInfo](Localization("population.literacy"),
    x => DoubleFormatter().format(x.literacy * 100) + "%")

  private val partyColumn = new TableColumn[StateInfo, PartyComponentColorName] {
    text = Localization("parliament.rulingParty")
    cellFactory = p => new TableCell[StateInfo, PartyComponentColorName] {
      override def updateItem(t: PartyComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty {
        new PartyComponentColorName(p.value.rulingParty)
      }
    }
    editable = false
  }

  private val cultureColumn = new TableColumn[StateInfo, CultureComponentColorName] {
    text = Localization("culture")
    cellFactory = p => new TableCell[StateInfo, CultureComponentColorName] {
      override def updateItem(t: CultureComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty {
        new CultureComponentColorName(p.value.state.primeCulture)
      }
    }
    editable = false
  }

  statesTable.columns ++= List(stateColumn, cultureColumn, relationsColumn, armyColumn, literacyColumn,
    partyColumn, moneyReservesColumn, incomeColumn, spendingColumn)

  private val buffer = new ObservableBuffer[StateInfo]()
  val relationships = actions.relationships(currentState)
  val stateInfos = actions.stateInfo.filter(_.state == currentState) :::
    actions.stateInfo.filterNot(_.state == currentState).sortBy(a => -actions.relationships(a.state)(currentState))
  statesTable.items = buffer ++ stateInfos

  statesTable.delegate.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
  statesTable.delegate.getSelectionModel.clearAndSelect(0)
  val selectedItem: ReadOnlyObjectProperty[StateInfo] = statesTable.delegate.getSelectionModel.selectedItemProperty
  add(statesTable, "grow,push")
}

class StateDiplomacyPane(currentState: State, selectedStateInfo: StateInfo, actions: WorldStateDiplomacyActions, stage: Stage, parentFrame:WorldFrame) extends MigPane {
  private val selectedState = selectedStateInfo.state

  private val tabPane = new TabPane {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  val relationsTab = new Tab {
    text = Localization("diplomacy.relations")
    style = Components.largeFontStyle
    content = new MigPane {
      add(new StateRelationsPane(currentState, selectedState, actions), "wrap")
      add(new StateAgreementsPane(stage, selectedState, actions), "grow, push")
    }
  }

  val actionsPane = new Tab {
    text = Localization("diplomacy.actions")
    style = Components.largeFontStyle
    content = if (currentState == selectedState) new Pane().delegate
    else new StateActionsPane(currentState, selectedState, actions, stage, parentFrame)
  }

  val claimsPane = new Tab {
    text = Localization("diplomacy.claims")
    style = Components.largeFontStyle
    content = new MigPane {
      add(BigText(Localization("diplomacy.theirClaims", selectedState.name)), "center, wrap")
      add(new ClaimsTable(actions.claims(selectedState)), "grow, push, wrap")
      add(BigText(Localization("diplomacy.claimsAgainst", selectedState.name)), "center, wrap")
      add(new ClaimsTable(actions.claimsAgainst(selectedState)), "grow, push")
    }
  }

  tabPane.tabs.addAll(relationsTab, actionsPane, claimsPane)
  add(BigText(selectedState.name), "wrap")
  add(tabPane, "grow, push")
}

class StateRelationsPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends MigPane() {
  add(new MigPane {
    add(MediumText(Localization("diplomacy.reputation") + ":"))
    add(BigText(reputationText))
  }, "wrap")

  if (selectedState != currentState) {
    add(new MigPane {
      add(BigText(Localization("diplomacy.relationsToUs")))
      add(BigText(actions.relationships(selectedState)(currentState).toString), "wrap")
      actions.relationshipsDescribed(selectedState).getOrElse(currentState, Nil).foreach { b =>
        add(MediumText(b.title))
        add(MediumText(b.bonus.toString), "wrap")
      }
    })
  }

  def reputationText: String = {
    val badBoy = actions.diplomacyEngine.badBoy.getOrElse(currentState, 0d)
    Localization(WorldConstants.Diplomacy.reputationDescriptionTextKey(badBoy)) + s" ($badBoy)"
  }
}

class StateAgreementsPane(stage: Stage, selectedState: State, actions: WorldStateDiplomacyActions) extends MigPane() {

  private val agreements = actions.agreements(selectedState)
  private val vassalAgreements = agreements.collect { case v: VassalAgreement if v.vassal == selectedState => v }
  private val overlordAgreements = agreements.collect { case v: VassalAgreement if v.overlord == selectedState => v }
  private val allianceAgreements = agreements.collect { case aa: AllianceAgreement => aa }
  private val wars = agreements.collect { case w: WarAgreement => w }
  private val truces = agreements.collect { case t: TruceAgreement => t }

  private def addWar(war: WarAgreement): Unit = {
    add(new WarPane(stage, war, actions), "grow, push, wrap")
  }

  if (vassalAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.vassalOf")), "wrap")
    add(new StateComponentList(vassalAgreements.map(_.overlord)), "wrap")
  }

  if (overlordAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.overlordFor")), "wrap")
    add(new StateComponentList(overlordAgreements.map(_.vassal)), "wrap")
  }

  if (allianceAgreements.nonEmpty) {
    add(MediumText(Localization("diplomacy.alliances")), "wrap")
    val alliances = allianceAgreements.flatMap(_.sides - selectedState)
    add(new StateComponentList(alliances), "wrap")
  }

  if (truces.nonEmpty) {
    add(MediumText(Localization("diplomacy.truces")), "wrap")
    val currentTruces = truces.flatMap(_.sides - selectedState)
    add(new StateComponentList(currentTruces), "wrap")
  }

  if (wars.nonEmpty) {
    add(MediumText(Localization("diplomacy.wars")), "wrap")
    wars.foreach(addWar)
  }
}

class StateActionsPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions, stage: Stage, parentFrame:WorldFrame) extends MigPane {

  import ModalDialog._

  private val declareWar = new MediumButton() {
    text = Localization("diplomacy.declareWar.button")
    onAction = { _ =>
      val dialog = new DeclareWarPane(currentState, selectedState, actions).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message)
        parentFrame.showDiplomacyPane()
      }
    }
  }

  private val addWarTarget = new MediumButton {
    text = Localization("diplomacy.addWarTarget")
    onAction = { _ =>
      actions.warsForWhichCanAddTarget(currentState, selectedState).headOption.foreach { wa =>
        val dialog = new AddWarTargetPane(currentState, selectedState, wa, actions).showDialog(stage)
        dialog.dialogResult.foreach { wt =>
          actions.diplomacyEngine.addWarTarget(wa, wt)
          parentFrame.showDiplomacyPane()
        }
      }
    }
  }

  private val proposePeace = new MediumButton() {
    text = Localization("diplomacy.proposePeace.button")
    onAction = { _ =>
      val dialog = new ProposePeacePane(currentState, selectedState, actions, false).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message.right.get)
        parentFrame.showDiplomacyPane()
      }
    }
  }

  private val proposeSeparatePeace = new MediumButton {
    text = Localization("diplomacy.proposeSeparatePeace.button")
    onAction = { _ =>
      val dialog = new ProposePeacePane(currentState, selectedState, actions, true).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message.left.get)
        parentFrame.showDiplomacyPane()
      }
    }
  }

  private val proposeVassalization = new MediumButton() {
    text = Localization("diplomacy.proposeVassalization.button")
    onAction = { _ =>
      actions.sendMessage(new VassalizationProposal(currentState, selectedState))
      parentFrame.showDiplomacyPane()
    }
  }

  private val proposeOverlordship = new MediumButton() {
    text = Localization("diplomacy.proposeOverlordship.button")
    onAction = { _ =>
      actions.sendMessage(new OverlordshipProposal(currentState, selectedState))
      parentFrame.showDiplomacyPane()
    }
  }

  private val proposeAlliance = new MediumButton() {
    text = Localization("diplomacy.proposeAlliance.button")
    onAction = { _ =>
      actions.sendMessage(new AllianceProposal(currentState, selectedState))
      parentFrame.showDiplomacyPane()
    }
  }

  private val callAllyToWarButtons = actions.allyCanJoinWars(currentState, selectedState).map { war =>
    new MediumButton {
      text = Localization("diplomacy.callAlly", war.oppositeLeader(currentState, actions.diplomacyEngine).name)
      onAction = { _ =>
        actions.sendMessage(new AskJoinWar(currentState, selectedState, war))
        parentFrame.showDiplomacyPane()
      }
    }
  }

  if (actions.canProposePeace(currentState, selectedState)) {
    add(proposePeace, "wrap")
  }

  if (actions.canProposeSeparatePeace(currentState, selectedState)) {
    add(proposeSeparatePeace, "wrap")
  }

  if (actions.canDeclareWar(currentState, selectedState)) {
    add(declareWar, "wrap")
  }

  if (actions.inWarAgainst(currentState, selectedState)) {
    add(addWarTarget, "wrap")
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

  if (callAllyToWarButtons.nonEmpty) {
    callAllyToWarButtons.foreach(add(_, "wrap"))
  }
}

class DeclareWarPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends DialogStage[DeclareWar] {

  private lazy val selectWarTarget = new SelectWarTarget(actions.diplomacyEngine.possibleTargetsForStartingWar(currentState, selectedState))

  override def onOkButtonPressed(): Unit = {
    super.onOkButtonPressed()
    dialogResult = selectWarTarget.selectedWarTarget.map { wt =>
      new DeclareWar(currentState, selectedState, wt, Set())
    }
  }

  override protected def dialogContent: Region = new MigPane("") {
    add(BigText(Localization("diplomacy.declareWarOn", selectedState.name)), "wrap")
    add(selectWarTarget, "grow, push")
  }

  override protected def css: Option[String] = None
}

class AddWarTargetPane(currentState: State, selectedState: State, warAgreement: WarAgreement, actions:WorldStateDiplomacyActions) extends DialogStage[WarTarget] {

  private lazy val selectWarTarget = new SelectWarTarget(actions.diplomacyEngine.possibleWarTargets(warAgreement, currentState))

  override def onOkButtonPressed(): Unit = {
    super.onOkButtonPressed()
    dialogResult = selectWarTarget.selectedWarTarget
  }

  override protected def dialogContent: Region = new MigPane {
    add(BigText(Localization("diplomacy.addWarTargetTo", selectedState.name)), "wrap")
    add(selectWarTarget, "grow, push")
  }

  override protected def css: Option[String] = None
}

class SelectWarTarget(possibleWarTargets:Set[WarTarget]) extends MigPane {

  private abstract class WarTargetSelection extends BorderPane {
    def selectedWarTarget: Option[WarTarget]
  }

  private trait WarTargetKind {
    def component: Option[WarTargetSelection]

    def label: String
  }

  private class VassalizeTargetKind extends WarTargetKind {
    private val possibleVassalizationTargets = possibleWarTargets.collect {
      case v:Vassalize => v
    }

    override def component: Option[WarTargetSelection] = {
      val child = new WarTargetSelection {
        val tg = new ToggleGroup()
        center = new MigPane {
          possibleVassalizationTargets.foreach { pt =>
            val rb = new RadioButton {
              text = pt.giver.name
              toggleGroup = tg
              userData = pt
              style = Components.largeFontStyle
            }
            add(rb, "wrap")
          }
        }

        override def selectedWarTarget: Option[WarTarget] = Option(tg.selectedToggle).flatMap { st =>
          Option(st.value).map { st =>
            st.userData.asInstanceOf[Vassalize]
          }
        }
      }

      if (possibleVassalizationTargets.isEmpty) None else Some(child)
    }

    override def label: String = Localization("diplomacy.vassalize")
  }

  private class DemandMoneyTargetKind extends WarTargetKind {
    private val possibleDemandMoneyTargets = possibleWarTargets.collect {
      case t:TakeMoney => t
    }

    override def component: Option[WarTargetSelection] = {
      val child = new WarTargetSelection {
        val tg = new ToggleGroup()
        center = new MigPane {
          possibleDemandMoneyTargets.foreach { pt =>
            val rb = new RadioButton {
              text = pt.giver.name
              toggleGroup = tg
              userData = pt
              style = Components.largeFontStyle
            }
            add(rb, "wrap")
          }
        }

        override def selectedWarTarget: Option[WarTarget] = Option(tg.selectedToggle).flatMap { st =>
          Option(st.value).map { st =>
            st.userData.asInstanceOf[TakeMoney]
          }
        }
      }

      if (possibleDemandMoneyTargets.isEmpty) None else Some(child)
    }

    override def label: String = Localization("diplomacy.demand.money")
  }

  private class TakeProvinceTargetKind extends WarTargetKind {
    private val possibleProvinces = possibleWarTargets.collect { case t: TakeProvince => t }

    override def component: Option[WarTargetSelection] = if (possibleProvinces.nonEmpty) {
      Some(new WarTargetSelection {
        private val group = new ToggleGroup()

        override def selectedWarTarget: Option[WarTarget] = Option(group.selectedToggle).flatMap { st =>
          Option(st.value).map { toggle =>
            toggle.getUserData.asInstanceOf[TakeProvince]
          }
        }

        center = new MigPane {
          possibleProvinces.foreach { p =>
            val radio = new RadioButton {
              text = p.province.name
              toggleGroup = group
              userData = p
              style = Components.largeFontStyle
            }
            add(radio, "wrap")
          }
        }
      })
    } else None

    override def label: String = Localization("diplomacy.takeProvince")
  }

  private class CollapseTargetKind extends WarTargetKind {
    private val possibleTargets = possibleWarTargets.collect {
      case c: CrackState => c
    }

    override def component: Option[WarTargetSelection] = if (possibleTargets.nonEmpty) {
      Some(
        new WarTargetSelection {
          private val group = new ToggleGroup()

          override def selectedWarTarget: Option[WarTarget] = Option(group.selectedToggle).flatMap { st =>
            Option(st.value).map { toggle =>
              toggle.getUserData.asInstanceOf[CrackState]
            }
          }

          center = new MigPane {
            possibleTargets.foreach { s =>
              val radio = new RadioButton {
                text = s.giver.name
                toggleGroup = group
                userData = s
                style = Components.largeFontStyle
              }
              add(radio, "wrap")
            }
          }
        }
      )
    } else None

    override def label: String = Localization("diplomacy.collapseState")
  }

  private class LiberateCultureTargetKind extends WarTargetKind {
    private val possibleCultures = possibleWarTargets.collect {
      case lc: LiberateCulture => lc.culture
    }

    override def component: Option[WarTargetSelection] = if (possibleCultures.nonEmpty) {
      Some(
        new WarTargetSelection {
          private val group = new ToggleGroup()

          override def selectedWarTarget: Option[WarTarget] = Option(group.selectedToggle).flatMap { st =>
            Option(st.value).map { toggle =>
              toggle.getUserData.asInstanceOf[LiberateCulture]
            }
          }

          center = new MigPane {
            possibleCultures.foreach { s =>
              val radio = new RadioButton {
                text = s.name
                toggleGroup = group
                userData = s
                style = Components.largeFontStyle
              }
              add(radio, "wrap")
            }
          }
        }
      )
    } else None

    override def label: String = Localization("diplomacy.liberateCulture")
  }

  private def possibleWarTargetKinds: List[WarTargetKind] = {
    List(new VassalizeTargetKind,
      new DemandMoneyTargetKind,
      new TakeProvinceTargetKind,
      new CollapseTargetKind,
      new LiberateCultureTargetKind).filter(_.component.nonEmpty)
  }

  private class RadioTargetKindsPane(kinds: List[WarTargetKind]) extends MigPane("") {
    private val group = new ToggleGroup()

    kinds.foreach { k =>
      val btn = new RadioButton(k.label) {
        toggleGroup = group
        userData = k
        style = Components.largeFontStyle
      }
      add(btn, "wrap")
    }

    val selectedWarTargetKind: ObjectProperty[WarTargetKind] = group.selectedToggle.map { t =>
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

  add(left)
  add(right)

  def selectedWarTarget: Option[WarTarget] = right.centerComponent.value.flatMap(_.selectedWarTarget)
}

class ProposePeacePane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions, separatePeace: Boolean) extends DialogStage[Either[ProposePeace, ProposeSeparatePeace]] {

  // TODO case when two simultaneous wars, however it is very unlikely
  private lazy val warOpt = actions.warsForWhichCanProposePeace(currentState, selectedState, separatePeace).headOption

  lazy val targets: List[WarTarget] = {
    warOpt.map { war =>
      if (separatePeace) {
        if (war.isLeader(currentState, actions.diplomacyEngine) && !war.isLeader(selectedState, actions.diplomacyEngine)) {
          val affected = war.sideByState(currentState) + selectedState ++ actions.diplomacyEngine.getVassals(selectedState)
          war.targets.filter(t => affected.contains(t.giver) && affected.contains(t.demander)).toList
        } else if (!war.isLeader(currentState, actions.diplomacyEngine) && war.isLeader(selectedState, actions.diplomacyEngine)) {
          val affected = war.sideByState(selectedState) + currentState ++ actions.diplomacyEngine.getVassals(currentState)
          war.targets.filter(t => affected.contains(t.giver) && affected.contains(t.demander)).toList
        } else {
          sys.error(s"Invalid separate peace for war $war and currentState $currentState and selectedState $selectedState")
        }
      } else {
        war.targets.toList
      }
    }.getOrElse(Nil)
  }

  private var selectedTargets: Set[WarTarget] = Set()

  private def checkButtons = targets.map { t =>
    new CheckBox(t.localizeTarget) {
      onAction = { _ =>
        if (this.selected.value) selectedTargets += t
        else selectedTargets -= t
        warOpt.foreach { war =>
          dialogResult = Some(
            if (separatePeace) {
              val separateState = if (war.isLeader(currentState, actions.diplomacyEngine) && !war.isLeader(selectedState, actions.diplomacyEngine)) {
                selectedState
              } else if (!war.isLeader(currentState, actions.diplomacyEngine) && war.isLeader(selectedState, actions.diplomacyEngine)) {
                currentState
              } else {
                sys.error(s"Invalid separate peace for war $war and currentState $currentState and selectedState $selectedState")
              }
              Right(ProposeSeparatePeace(currentState, selectedState, war, selectedTargets, separateState))
            }
            else
              Left(ProposePeace(currentState, selectedState, war, selectedTargets))
          )
        }
      }
    }
  }

  override def dialogContent: Region = new MigPane {
    checkButtons.foreach { cb =>
      add(cb, "wrap")
    }
  }

  override val css: Option[String] = None

}

class StateComponentColorName(state: State, align: String = "left") extends MigPane(align) with WorldInterfaceJavaNode {
  val rect = Rectangle(Components.mediumFontSize * 2, Components.mediumFontSize * 2)
  rect.fill = state.color
  rect.stroke = Color.Black
  val text = BigText(state.name)
  add(rect)
  add(text)
}

class StateComponentList(list: List[State]) extends MigPane {

  private def buildStateColor(s: State): StateComponentColor = new StateComponentColor(s)

  private def buildStateColorName(s: State): StateComponentColorName = new StateComponentColorName(s)

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

class ClaimsTable(claims: List[Claim]) extends TableView[Claim] {
  this.style = Components.mediumFontStyle

  val stateColumn = new TableColumn[Claim, StateComponentColorName] {

    text = Localization("diplomacy.claimant")

    cellFactory = p => new TableCell[Claim, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      val node = p.value match {
        case claim: Claim.ProvinceClaim => new StateComponentColorName(claim.state)
        case Claim.VassalizationClaim(state, _, _) => new StateComponentColorName(state)
      }
      ObjectProperty(node)
    }
    editable = false
  }

  val targetStateColumn = new TableColumn[Claim, StateComponentColorName] {

    text = Localization("diplomacy.claimTarget")

    cellFactory = p => new TableCell[Claim, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      val node = p.value match {
        case claim: Claim.ProvinceClaim => new StateComponentColorName(claim.targetState)
        case Claim.VassalizationClaim(_, possibleVassal, _) => new StateComponentColorName(possibleVassal)
      }
      ObjectProperty(node)
    }
    editable = false
  }

  val provinceOrVassalize = new TableColumn[Claim, String] {
    text = Localization("diplomacy.claim")

    cellValueFactory = p => StringProperty {
      p.value match {
        case claim: Claim.ProvinceClaim => claim.province.name
        case Claim.VassalizationClaim(state, _, _) => Localization("diplomacy.vassalize")
      }
    }
    editable = false
  }

  val claimEnd = new TableColumn[Claim, String] {
    text = Localization("diplomacy.validUntil")

    cellValueFactory = p => StringProperty {
      p.value match {
        case _: Claim.StrongProvinceClaim => ""
        case claim: Claim.WeakProvinceClaim => Seasons.date(claim.claimTurnEnd).localizedString
        case Claim.VassalizationClaim(_, _, claimTurnEnd) => Seasons.date(claimTurnEnd).localizedString
      }
    }
    editable = false
  }

  columns ++= List(stateColumn, targetStateColumn, provinceOrVassalize, claimEnd)

  items = new ObservableBuffer[Claim]() ++ claims
}

class WarPane(stage: Stage, war: WarAgreement, diplomacyActions: WorldStateDiplomacyActions) extends MigPane {
  add(BigText(war.fullWarName), "center")
  add(showBattlesButton, "center, wrap")
  add(buildTable, "grow, push, span 2")

  private case class WarTableRow(attacker: Option[State], warTarget: Option[WarTarget], defender: Option[State])

  private def tableRows: List[WarTableRow] = {
    val mainAttacker = war.attackersLeader(diplomacyActions.diplomacyEngine)
    val mainDefender = war.defendersLeader(diplomacyActions.diplomacyEngine)

    val allAttackers = mainAttacker :: (war.attackers - mainAttacker).toList
    val allDefenders = mainDefender :: (war.defenders - mainDefender).toList
    val targets = war.targets.toList

    val infiniteNoneStream = Stream.continually(None)

    val attackersStream = allAttackers.map(Some.apply).toStream ++ infiniteNoneStream
    val defendersStream = allDefenders.map(Some.apply).toStream ++ infiniteNoneStream
    val targetsStream = targets.map(Some.apply).toStream ++ infiniteNoneStream

    attackersStream.zip(defendersStream).zip(targetsStream).map {
      case ((x, y), z) => (x, y, z)
    }.takeWhile {
      case (None, None, None) => false
      case _ => true
    }.map { case (a, d, t) =>
      WarTableRow(a, t, d)
    }.toList
  }

  private def buildTable: TableView[WarTableRow] = {

    class WarColumn(f: WarTableRow => Option[State]) extends TableColumn[WarTableRow, MigPane] {

      cellFactory = p => new TableCell[WarTableRow, MigPane] {
        override def updateItem(t: MigPane, b: Boolean): Unit = {
          super.updateItem(t, b)
          setGraphic(t)
        }
      }
      cellValueFactory = p => {
        val node = f(p.value) match {
          case Some(x) => new StateComponentColorName(x)
          case None => new MigPane()
        }
        ObjectProperty(node)
      }
      editable = false
    }

    val attacker = new WarColumn(_.attacker) {
      text = Localization("diplomacy.attackers")
    }

    val defender = new WarColumn(_.defender) {
      text = Localization("diplomacy.defenders")
    }

    val demander = new WarColumn(_.warTarget.map(_.demander)) {
      text = Localization("diplomacy.demander")
    }

    val giver = new WarColumn(_.warTarget.map(_.giver)) {
      text = Localization("diplomacy.giver")
    }

    val target = new TableColumn[WarTableRow, String] {
      text = Localization("diplomacy.warTarget")

      cellValueFactory = p => StringProperty {
        p.value.warTarget match {
          case Some(x) => x.localizeTarget
          case None => ""
        }
      }
      editable = false
    }

    val tableView = new TableView[WarTableRow] {
      style = Components.mediumFontStyle
    }

    tableView.columns ++= List(attacker, target, defender)
    tableView.items = ObservableBuffer() ++ tableRows
    tableView

  }

  def showBattlesButton: Node = {
    val pane = new ScrollPane {
      prefHeight <== stage.height - 50
      content = new BattleReportPane(war.battles)
    }

    val dialog = new Stage {
      title = Localization("battleReport.title")
      scene = new Scene {
        content = pane
      }
    }

    new BigButton {
      text = Localization("battleReport.battles")
      onAction = { _ =>
        import ModalDialog._
        dialog.showDialog(stage)
      }
      disable = war.battles.isEmpty
    }
  }
}