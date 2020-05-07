package mr.merc.ui.world

import mr.merc.economics.{Culture, Seasons, WorldStateDiplomacyActions}
import mr.merc.politics.{Province, State}
import mr.merc.util.MercTooltip
import org.tbee.javafx.scene.layout.MigPane
import javafx.scene.control.{SelectionMode, TableCell}
import mr.merc.diplomacy.Claim
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
import scalafx.scene.control.TabPane.TabClosingPolicy

import scala.util.Try

class DiplomacyPane(actions: WorldStateDiplomacyActions, currentState: State, stage: Stage) extends PaneWithTwoHorizontalChildren(0.2) {

  private val tablePane = new StatesTablePane(actions, currentState)
  private val property = tablePane.selectedItem
  private val selectedPane = new BorderPane {
    center <== property.map(Option.apply).map {
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

  /*
  width.onChange { (_, _, _) =>
    val header = lookup("TableHeaderRow").delegate.asInstanceOf[javafx.scene.layout.Pane]
    header.setMinHeight(0)
    header.setPrefHeight(0)
    header.setMaxHeight(0)
    header.setVisible(false)
  }
   */

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

class StateDiplomacyPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions, stage: Stage) extends MigPane {
  private val tabPane = new TabPane {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  val relationsTab = new Tab {
    text = Localization("diplomacy.relations")
    style = Components.largeFontStyle
    content = new StateRelationsPane(currentState, selectedState, actions)
  }

  val agreementsPane = new Tab {
    text = Localization("diplomacy.agreements")
    style = Components.largeFontStyle
    content = new StateAgreementsPane(selectedState, actions)
  }

  val actionsPane = new Tab {
    text = Localization("diplomacy.actions")
    style = Components.largeFontStyle
    content = if (currentState == selectedState) new Pane().delegate
    else new StateActionsPane(currentState, selectedState, actions, stage)
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

  tabPane.tabs.addAll(relationsTab, agreementsPane, actionsPane, claimsPane)
  add(BigText(selectedState.name), "wrap")
  add(tabPane, "grow, push")
}

class StateRelationsPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends MigPane() {
  if (selectedState != currentState) {
    add(BigText(Localization("diplomacy.relationsToUs")))
    add(BigText(actions.relationships(selectedState)(currentState).toString), "wrap")
    actions.relationshipsDescribed(selectedState).getOrElse(currentState, Nil).foreach { b =>
      add(MediumText(b.title))
      add(MediumText(b.bonus.toString), "wrap")
    }
  }
}

class StateAgreementsPane(selectedState: State, actions: WorldStateDiplomacyActions) extends MigPane() {

  private val agreements = actions.agreements(selectedState)
  private val vassalAgreements = agreements.collect { case v: VassalAgreement if v.vassal == selectedState => v }
  private val overlordAgreements = agreements.collect { case v: VassalAgreement if v.overlord == selectedState => v }
  private val allianceAgreements = agreements.collect { case aa: AllianceAgreement => aa }
  private val wars = agreements.collect { case w: WarAgreement => w }
  private val truces = agreements.collect { case t: TruceAgreement => t }

  private def addWar(war: WarAgreement): Unit = {
    add(new WarPane(war, actions), "grow, push, span 2, wrap")
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

  if (wars.nonEmpty) {
    add(MediumText(Localization("diplomacy.wars")), "span 2, wrap")
    wars.foreach(addWar)
  }
}

class StateActionsPane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions, stage: Stage) extends MigPane {

  import ModalDialog._

  private val declareWar = new MediumButton() {
    text = Localization("diplomacy.declareWar")
    onAction = { _ =>
      val dialog = new DeclareWarPane(currentState, selectedState, actions).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message)
      }
    }
  }

  private val proposePeace = new MediumButton() {
    text = Localization("diplomacy.proposePeace")
    onAction = { _ =>
      val dialog = new ProposePeacePane(currentState, selectedState, actions).showDialog(stage)
      dialog.dialogResult.foreach { message =>
        actions.sendMessage(message)
      }
    }
  }

  private val proposeVassalization = new MediumButton() {
    text = Localization("diplomacy.proposeVassalization")
    onAction = { _ =>
      actions.sendMessage(new VassalizationProposal(currentState, selectedState))
    }
  }

  private val proposeOverlordship = new MediumButton() {
    text = Localization("diplomacy.proposeOverlordship")
    onAction = { _ =>
      actions.sendMessage(new OverlordshipProposal(currentState, selectedState))
    }
  }

  private val proposeAlliance = new MediumButton() {
    text = Localization("diplomacy.proposeAlliance")
    onAction = { _ =>
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

class SelectWarTarget(currentState: State, selectedState: State, alreadyExistedTargets: Set[WarTarget], actions: WorldStateDiplomacyActions) extends PaneWithTwoHorizontalChildren {

  private abstract class WarTargetSelection extends BorderPane {
    def selectedWarTarget: Option[WarTarget]
  }

  private trait WarTargetKind {
    def component: Option[WarTargetSelection]

    def label: String
  }

  private class VassalizeTargetKind extends WarTargetKind {
    private val possibleVassalizationTargets = actions.possibleVassalizationWarTargets(selectedState).toSet --
      alreadyExistedTargets.collect { case v: Vassalize => v.giver }

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

        override def selectedWarTarget: Option[WarTarget] = tg.selectedToggle.map { state =>
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
      override def selectedWarTarget: Option[WarTarget] = Try(textField.text.value.toDouble).map { d =>
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
      alreadyExistedTargets.collect { case t: TakeProvince => t.province }

    override def component: Option[WarTargetSelection] = if (possibleProvinces.nonEmpty) {
      Some(new WarTargetSelection {
        private val group = new ToggleGroup()

        override def selectedWarTarget: Option[WarTarget] = group.selectedToggle.map(Option.apply).map {
          case Some(toggle) =>
            val province = toggle.getUserData.asInstanceOf[Province]
            Some(new TakeProvince(currentState, selectedState, province))
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
      case c: CrackState => c.giver
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
      )
    } else None

    override def label: String = Localization("diplomacy.collapseState")
  }

  private class LiberateCultureTargetKind extends WarTargetKind {
    private val possibleCultures = actions.possibleCulturesToLiberate(selectedState) -- alreadyExistedTargets.collect {
      case lc: LiberateCulture => lc.culture
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

  private class RadioTargetKindsPane(kinds: List[WarTargetKind]) extends MigPane {
    private val group = new ToggleGroup()

    kinds.foreach { k =>
      val btn = new RadioButton {
        text = k.label
        toggleGroup = group
        userData = k

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

  setTwoChildren(left, right)

  val selectedWarTarget: ObjectProperty[Option[WarTarget]] = right.centerComponent.map(_.flatMap(_.selectedWarTarget))
}

class ProposePeacePane(currentState: State, selectedState: State, actions: WorldStateDiplomacyActions) extends DialogStage[ProposePeace] {

  // TODO case when two simultaneous wars, however it is very unlikely
  private val warOpt = actions.warsForWhichCanProposePeace(currentState, selectedState).headOption

  def targets: List[WarTarget] = {
    warOpt.map { war =>
      val currentSide = if (war.attackers.contains(currentState)) war.attackers
      else if (war.defenders.contains(currentState)) war.defenders
      else sys.error(s"Impossible propose peace for $currentState in war $war")

      war.targets.filter { target =>
        currentSide.contains(target.demander)
      }.toList
    }.getOrElse(Nil)
  }

  private var selectedTargets: Set[WarTarget] = Set()

  private val checkButtons = targets.map { t =>
    new CheckBox(t.localizeTarget) {
      onAction = { _ =>
        if (this.selected.value) selectedTargets += t
        else selectedTargets -= t
        warOpt.foreach { war =>
          dialogResult = Some(new ProposePeace(currentState, selectedState, war, selectedTargets))
        }
      }
    }
  }

  override val dialogContent: Node = new MigPane {
    checkButtons.foreach { cb =>
      add(cb, "wrap")
    }
  }

  override val css: Option[String] = None

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

class WarPane(war:WarAgreement, diplomacyActions: WorldStateDiplomacyActions) extends MigPane {
  add(BigText(war.fullWarName), "center, wrap")
  add(buildTable, "grow, push")

  private case class WarTableRow(attacker:Option[State], warTarget:Option[WarTarget], defender: Option[State])

  private def tableRows:List[WarTableRow] = {
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

  private def buildTable:TableView[WarTableRow] = {

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

    tableView.columns ++= List(attacker, demander, target, giver, defender)
    tableView.items = ObservableBuffer() ++ tableRows
    tableView

  }
}