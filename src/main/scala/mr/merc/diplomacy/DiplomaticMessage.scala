package mr.merc.diplomacy

import mr.merc.diplomacy.Claim.VassalizationClaim
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, FriendshipAgreement, SanctionAgreement, TruceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.RelationshipEvent._
import mr.merc.local.Localization
import mr.merc.politics.State
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.ui.world.{BigText, StateComponentColorName}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.Node
import scalafx.Includes._

sealed trait DiplomaticMessage {
  def from: State

  def to: State

  def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean

  def messageTitle: String

  def body: String

  def beforeSendAction(diplomacy:WorldDiplomacy, currentTurn:Int):Unit = {}

  def renderInReport: Option[Node]

  def shouldRefreshMapAfterAnswer:Boolean
}

trait DiplomaticDeclaration extends DiplomaticMessage {
  def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit
}

trait DiplomaticProposal extends DiplomaticMessage {

  def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit

  def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit
}

sealed trait CustomDiplomaticQuestion extends DiplomaticMessage {
  def defaultOk(diplomacy: WorldDiplomacy, currentTurn: Int): Unit
}

object DiplomaticMessage {

  class AllianceProposal(val from: State, val to: State) extends DiplomaticProposal {
    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.dropAllClaimsAgainst(to, from)
      diplomacy.addAgreement(new AllianceAgreement(from, to, currentTurn))
      diplomacy.sendMessage(new AllianceAccepted(to, from), currentTurn)
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.addEvent(new DeclinedAlliance(from, to, currentTurn))
      diplomacy.sendMessage(new AllianceRejected(to, from), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val agreements = diplomacy.agreementsAnd(from) ::: diplomacy.agreementsAnd(to)
      !agreements.exists {
        case v: VassalAgreement if Set(from, to).contains(v.vassal) => true
        case aa: AllianceAgreement if aa.sides == Set(from, to) => true
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to))
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.proposeAlliance.title", from.name)

    override def body: String = Localization("diplomacy.proposeAlliance.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class AllianceAccepted(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.acceptedAlliance.title", from.name)

    override def body: String = Localization("diplomacy.acceptedAlliance.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.acceptedAlliance")))
      add(new StateComponentColorName(from))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class AllianceRejected(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.rejectedAlliance.title", from.name)

    override def body: String = Localization("diplomacy.rejectedAlliance.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class FriendshipProposal(val from: State, val to: State) extends DiplomaticProposal {
    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.dropAllClaimsAgainst(to, from)
      diplomacy.addAgreement(new FriendshipAgreement(from, to, currentTurn))
      diplomacy.sendMessage(new FriendshipAccepted(to, from), currentTurn)
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.addEvent(new RejectedFriendship(from, to, currentTurn))
      diplomacy.sendMessage(new FriendshipRejected(to, from), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val agreements = diplomacy.agreementsAnd(from) ::: diplomacy.agreementsAnd(to)
      !agreements.exists {
        case v: VassalAgreement if Set(from, to).contains(v.vassal) => true
        case aa: AllianceAgreement if aa.sides == Set(from, to) => true
        case fa: FriendshipAgreement if fa.sides == Set(from, to) => true
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to))
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.proposeFriendship.title", from.name)

    override def body: String = Localization("diplomacy.proposeFriendship.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class FriendshipAccepted(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.acceptedFriendship.title")

    override def body: String = Localization("diplomacy.acceptedFriendship.body")

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.acceptedFriendship")))
      add(new StateComponentColorName(from))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class FriendshipRejected(val from: State, val to: State) extends DiplomaticDeclaration {

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.rejectedFriendship.title")

    override def body: String = Localization("diplomacy.rejectedFriendship.body")

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class VassalizationProposal(val from: State, val to: State) extends DiplomaticProposal {

    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.dropAllClaimsAgainst(to, from)
      diplomacy.addAgreement(new VassalAgreement(from, to, currentTurn))
      diplomacy.sendMessage(new VassalizationAccepted(to, from), currentTurn)
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.addEvent(new DeclinedVassalization(from, to, currentTurn))
      diplomacy.sendMessage(new VassalizationRejected(to, from), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val agreements = diplomacy.agreementsAnd(from) ::: diplomacy.agreementsAnd(to)
      !agreements.exists {
        case v: VassalAgreement if Set(v.overlord, v.vassal) == Set(from, to) => true
        case v: VassalAgreement if Set(from, to).contains(v.vassal) => true
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to))
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.proposeVassalization.title", from.name)

    override def body: String = Localization("diplomacy.proposeVassalization.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class VassalizationAccepted(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.acceptedVassalization.title", from.name)

    override def body: String = Localization("diplomacy.acceptedVassalization.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.acceptedVassalization")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class VassalizationRejected(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.rejectedVassalization.title", from.name)

    override def body: String = Localization("diplomacy.rejectedVassalization.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class OverlordshipProposal(val from: State, val to: State) extends DiplomaticProposal {
    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.dropAllClaimsAgainst(from, to)
      diplomacy.addAgreement(new VassalAgreement(to, from, currentTurn))
      diplomacy.sendMessage(new OverlordshipAccepted(to, from), currentTurn)
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.addEvent(new DeclinedOverlordship(from, to, currentTurn))
      diplomacy.sendMessage(new OverlordshipRejected(to, from), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val agreements = diplomacy.agreementsAnd(from) ::: diplomacy.agreementsAnd(to)
      !agreements.exists {
        case v: VassalAgreement if Set(v.overlord, v.vassal) == Set(from, to) => true
        case v: VassalAgreement if Set(from, to).contains(v.vassal) => true
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to))
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.proposeOverlordship.title", from.name)

    override def body: String = Localization("diplomacy.proposeOverlordship.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class OverlordshipAccepted(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.acceptedOverlordship.title", from.name)

    override def body: String = Localization("diplomacy.acceptedOverlordship.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.acceptedVassalization")))
      add(new StateComponentColorName(from))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class OverlordshipRejected(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.rejectedOverlordship.title", from.name)

    override def body: String = Localization("diplomacy.rejectedOverlordship.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class DeclareWar(val from: State, val to: State, target: WarTarget, attackerAllies:Set[State]) extends CustomDiplomaticQuestion {

    private var war:Option[WarAgreement] = None

    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      super.beforeSendAction(diplomacy, currentTurn)
      val name = diplomacy.generateNewWarName(from, to, target)
      val w = new WarAgreement(Set(from), Set(to), from, to, currentTurn, Set(target), name)
      diplomacy.addAgreement(w)
      war = Some(w)

      // TODO extract this code???
      target match {
        case w:TakeProvince => if (!diplomacy.hasClaimOverProvince(from, w.province)) {
          diplomacy.increaseBadBoy(from, NoCasusBelliWarBadBoy)
        }
        case lc:LiberateCulture => if (lc.culture != from.primeCulture) {
          diplomacy.increaseBadBoy(from, NoCasusBelliWarBadBoy)
        }
        case _:TakeMoney => diplomacy.increaseBadBoy(from, NoCasusBelliWarBadBoy)

        case v:Vassalize => if (!diplomacy.hasVassalizationClaimOverState(from, to)) {
          diplomacy.increaseBadBoy(from, NoCasusBelliWarBadBoy)
        }
      }

      callAlliesToJoin(from, diplomacy, currentTurn, attackerAllies)
    }

    override def defaultOk(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      require(war.nonEmpty, "War must be defined!")
      okAndCallAllies(diplomacy, currentTurn, Set())
    }

    def okAndCallAllies(diplomacy: WorldDiplomacy, currentTurn: Int, allies: Set[State]): Unit = {
      require(war.nonEmpty, "War must be defined!")
      val resultingAllies = allies ++ diplomacy.getOverlord(to)
      callAlliesToJoin(to, diplomacy, currentTurn, resultingAllies)
    }

    private def callAlliesToJoin(who:State, diplomacy: WorldDiplomacy, currentTurn: Int, allies: Set[State]): Unit = {
      allies.foreach { a =>
        if (diplomacy.areAllies(who, a)) {
          diplomacy.sendMessage(new AskJoinWar(who, a, war.get), currentTurn)
        } else if (diplomacy.isVassal(who, a)) {
          diplomacy.sendMessage(new OrderJoinWar(who, a, war.get),currentTurn)
        } else if (diplomacy.isVassal(a, who)) {
          diplomacy.sendMessage(new VassalKindlyAsksOverlordToJoinWar(who, a, war.get),currentTurn)
        }
      }
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val agreements = diplomacy.agreementsAnd(from) ::: diplomacy.agreementsAnd(to)

      !agreements.exists {
        case v: VassalAgreement if v.vassal == from => true
        case v: VassalAgreement if v.sides == Set(from, to) => true
        case aa: AllianceAgreement if aa.sides == Set(from, to) => true
        case fa: FriendshipAgreement if fa.sides == Set(from, to) => true
        case ta: TruceAgreement if ta.sides == Set(from, to) => false
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to)) && !war.contains(wa)
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.declareWar.title", from.name)

    override def body: String = Localization("diplomacy.declareWar.body", from.name, war.get.targets.map(_.localizeTarget).mkString("\n"))

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.declaredWar")))
      add(new StateComponentColorName(to), "wrap")
      add(BigText(Localization("messages.declaredWar.reason") + " " + target.localizeTarget), "span 3")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class AskJoinWar(val from: State, val to: State, val warAgreement: WarAgreement) extends DiplomaticProposal {
    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.joinWar(warAgreement, from, to, currentTurn)
      diplomacy.sendMessage(new AgreeJoinWar(to, from, warAgreement), currentTurn)
      diplomacy.addEvent(new AcceptedJoinWar(from, to, currentTurn))
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.sendMessage(new DeclineJoinWar(to, from, warAgreement), currentTurn)
      diplomacy.agreementsAnd(from, to).collectFirst {
        case aa: AllianceAgreement => aa
      }.foreach { a =>
        diplomacy.cancelAgreement(to, a, currentTurn)
        diplomacy.increaseBadBoy(from, RefuseAllyCallBadBoy)
      }
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      diplomacy.agreementsAnd(from).collectFirst {
        case wa: WarAgreement if Set(from, to).subsetOf(wa.sides) => wa
      }.isEmpty && diplomacy.agreementsAnd(from, to).collectFirst {
        case aa: AllianceAgreement => aa
      }.nonEmpty
    }

    override def messageTitle: String = Localization("diplomacy.askJoinWar.title", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false

    override def body: String = Localization("diplomacy.askJoinWar.body", from.name, warAgreement.localizeWar)
  }

  class OrderJoinWar(val from: State, val to: State, warAgreement: WarAgreement) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.joinWar(warAgreement, from, to, currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      diplomacy.agreementsAnd(from).collectFirst {
        case wa: WarAgreement if Set(from, to).subsetOf(wa.sides) => wa
      }.isEmpty && diplomacy.agreementsAnd(from, to).collectFirst {
        case va: VassalAgreement => va
      }.nonEmpty
    }

    override def messageTitle: String = Localization("diplomacy.orderJoinWar.title", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false

    override def body: String = Localization("diplomacy.orderJoinWar.body", from.name, warAgreement.localizeWar)
  }

  class VassalKindlyAsksOverlordToJoinWar(from: State, to: State, warAgreement: WarAgreement) extends OrderJoinWar(from, to, warAgreement) {

    override def messageTitle: String = Localization("diplomacy.kindlyAskToJoinWar.title", from.name)

    override def body: String = Localization("diplomacy.kindlyAskToJoinWar.body", from.name, warAgreement.localizeWar)
  }

  class AgreeJoinWar(val from: State, val to: State, warAgreement: WarAgreement) extends DiplomaticDeclaration {
    private val warCopy = warAgreement.clone()

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.agreeJoinWar.title", from.name)

    override def body: String = Localization("diplomacy.agreeJoinWar.body", from.name, warAgreement.localizeWar)

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.joinedWar.with")))
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.joinedWar.reason")), "wrap")
      add(BigText(Localization(warCopy.localizeWar)), "span 4")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class DeclineJoinWar(val from: State, val to: State, warAgreement: WarAgreement) extends DiplomaticDeclaration {
    private val warCopy = warAgreement.clone()

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.declineJoinWar.title", from.name)

    override def body: String = Localization("diplomacy.declineJoinWar.body", from.name, warAgreement.localizeWar)

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.declinedJoinWar.with")))
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.declinedJoinWar.reason")), "wrap")
      add(BigText(warCopy.localizeWar), "span 4")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false

  }

  case class ProposePeace(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget])
    extends DiplomaticProposal {
    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      val otherSides = warAgreement.sides - from - to
      diplomacy.acceptPeaceTreaty(warAgreement, acceptedTargets, currentTurn)
      diplomacy.sendMessage(new AcceptedPeaceProposal(to, from, warAgreement, acceptedTargets), currentTurn)
      otherSides.foreach { os =>
        diplomacy.sendMessage(new AcceptedOthersPeaceProposal(to, os, warAgreement, acceptedTargets), currentTurn)
      }
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.sendMessage(new DeclinedPeaceProposal(to, from, warAgreement, acceptedTargets), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean =
      diplomacy.wars.exists(wa => Set(wa.attackersLeader(diplomacy), wa.defendersLeader(diplomacy)) == Set(from, to))

    override def messageTitle: String = Localization("diplomacy.proposePeace.title", from.name)

    override def body: String = Localization("diplomacy.proposePeace.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class AcceptedPeaceProposal(val from: State, val to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget])
    extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.peaceAccepted.title", from.name)

    override def body: String = Localization("diplomacy.peaceAccepted.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.acceptedPeace.with")))
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.acceptedPeace.terms")), "wrap")
      add(BigText(WarAgreement.localizeTargetsList(acceptedTargets.toList)), "span 4")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = true
  }

  class AcceptedOthersPeaceProposal(val from:State, val to:State, warAgreement: WarAgreement, acceptedTargets:Set[WarTarget])
    extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.otherPeaceAccepted.title", from.name)

    override def body: String = Localization("diplomacy.otherPeaceAccepted.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.othersPeace")), "wrap")
      add(BigText(WarAgreement.localizeTargetsList(acceptedTargets.toList)), "span 2")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = true
  }

  class DeclinedPeaceProposal(val from: State, val to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget])
    extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.peaceDeclined.title", from.name)

    override def body: String = Localization("diplomacy.peaceDeclined.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  case class ProposeSeparatePeace(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget],
                             separateState:State) extends DiplomaticProposal {
    require(from == separateState || to == separateState)

    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      val sides = warAgreement.sideByState(separateState)
      val otherSides = warAgreement.sides - from - to
      diplomacy.acceptSeparatePeaceTreaty(warAgreement, separateState, acceptedTargets, currentTurn)
      (sides - separateState).foreach { left =>
        diplomacy.addEvent(new SeparatePeace(separateState, left, currentTurn))
      }
      diplomacy.sendMessage(AcceptedSeparatePeaceProposal(to, from, warAgreement, acceptedTargets, separateState), currentTurn)
      otherSides.foreach { os =>
        diplomacy.sendMessage(OtherAcceptedSeparatePeaceProposal(separateState, os, warAgreement, acceptedTargets), currentTurn)
      }
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.sendMessage(DeclinedSeparatePeaceProposal(to, from, warAgreement, acceptedTargets, separateState), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      val leaders = Set(warAgreement.defendersLeader(diplomacy), warAgreement.attackersLeader(diplomacy))
      val otherSides = warAgreement.sides -- leaders
      val vassals = otherSides.filter(s => diplomacy.getOverlord(s).nonEmpty)
      val possibleSeparateStates = otherSides -- vassals
      possibleSeparateStates.contains(separateState)
    }

    override def messageTitle: String = Localization("proposeSeparatePeace.title", separateState.name)

    override def body: String = if (from == separateState)
      Localization("proposeSeparatePeace.toIsLeader.body", from.name)
    else if (to == separateState)
      Localization("proposeSeparatePeace.fromIsLeader.body", from.name,
        WarAgreement.localizeTargetsList(acceptedTargets.toList))
    else sys.error(s"$separateState is not to [$to] and is not from [$from]")

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  case class AcceptedSeparatePeaceProposal(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget],
                                      separateState:State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.separatePeaceAccepted.title", from.name)

    override def body: String = Localization("diplomacy.separatePeaceAccepted.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.acceptedSeparatePeace")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = true
  }

  case class DeclinedSeparatePeaceProposal(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget],
                                      separateState:State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.separatePeaceDeclined.title", from.name)

    override def body: String = Localization("diplomacy.separatePeaceDeclined.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  case class OtherAcceptedSeparatePeaceProposal(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget]) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.otherSeparatePeaceAccepted.title", from.name)

    override def body: String = Localization("diplomacy.otherSeparatePeaceAccepted.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  case class StalledWarResolved(from: State, to: State, warAgreement: WarAgreement, acceptedTargets: Set[WarTarget]) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.stalledPeace.title", from.name)

    override def body: String = Localization("diplomacy.stalledPeace.body", from.name,
      WarAgreement.localizeTargetsList(acceptedTargets.toList))

    override def renderInReport: Option[Node] = Some(new MigPane{
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.acceptedPeace.with")))
      add(new StateComponentColorName(to))
      add(BigText(Localization("messages.acceptedPeace.terms")), "wrap")
      add(BigText(WarAgreement.localizeTargetsList(acceptedTargets.toList)), "span 4")
    })

    override def shouldRefreshMapAfterAnswer: Boolean = true
  }

  case class SanctionsEnacted(from: State, to: State) extends DiplomaticDeclaration {

    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.addAgreement(new SanctionAgreement(from, to, currentTurn))
    }

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      !diplomacy.agreementsAnd(from, to).exists {
        case _: VassalAgreement => true
        case _: AllianceAgreement => true
        case _: SanctionAgreement => true
        case wa: WarAgreement if wa.onDifferentSides(Set(from, to)) => true
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.sanctions.title")

    override def body: String = Localization("diplomacy.sanctions.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("diplomacy.sanctions.report")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  case class SanctionsStopped(from: State, to: State) extends DiplomaticDeclaration {

    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.agreementsAnd(from, to).collect {
        case ag: SanctionAgreement if ag.initiator == from && ag.underSanctions == to => ag
      }.foreach { da =>
        diplomacy.cancelAgreement(from, da, currentTurn)
      }
    }

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      diplomacy.agreementsAnd(from, to).collectFirst {
        case ag: SanctionAgreement if ag.initiator == from && ag.underSanctions == to => ag
      }.nonEmpty
    }

    override def messageTitle: String = Localization("diplomacy.cancelSanctions.title")

    override def body: String = Localization("diplomacy.cancelSanctions.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("diplomacy.cancelSanctions.report")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class VassalizationDemand(val from: State, val to: State) extends DiplomaticProposal {

    override def accept(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.dropAllClaimsAgainst(to, from)
      diplomacy.addAgreement(new VassalAgreement(from, to, currentTurn))
      diplomacy.sendMessage(new VassalizationAccepted(to, from), currentTurn)
    }

    override def decline(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      diplomacy.sendMessage(new VassalizationUltimatumRejected(to, from), currentTurn)
      diplomacy.sendMessage(new DeclareWar(from, to, Vassalize(from, to), Set()), currentTurn)
    }

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      !diplomacy.agreementsOr(from, to).exists {
        case v: VassalAgreement if Set(from, to).contains(v.vassal) => true
        case fa: FriendshipAgreement if Set(from, to) == fa.sides => true
        case wa: WarAgreement => wa.onDifferentSides(Set(from, to))
        case _ => false
      }
    }

    override def messageTitle: String = Localization("diplomacy.demandVassalization.title", from.name)

    override def body: String = Localization("diplomacy.demandVassalization.body", from.name)

    override def renderInReport: Option[Node] = None

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class VassalizationUltimatumRejected(val from: State, val to: State) extends DiplomaticDeclaration {
    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("diplomacy.demandVassalizationRejected.title", from.name)

    override def body: String = Localization("diplomacy.demandVassalizationRejected.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("messages.acceptedVassalizationDemand")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class DroppedClaim(val from: State, val to: State, claim: Claim) extends DiplomaticDeclaration {
    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      super.beforeSendAction(diplomacy, currentTurn)
      diplomacy.dropClaim(claim)
    }

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = true

    override def messageTitle: String = Localization("claims.drop.title")

    override def body: String = claim match {
      case claim: Claim.ProvinceClaim =>
        Localization("claims.drop.province.body", claim.state.name, claim.province.name)
      case Claim.VassalizationClaim(state, possibleVassal, claimTurnEnd) =>
        Localization("claims.drop.vassalization.body", state.name)
    }

    override def renderInReport: Option[Node] = claim match {
      case claim: Claim.ProvinceClaim => Some(new MigPane {
        add(new StateComponentColorName(from))
        add(BigText(Localization("claims.drop.province.render") + " "))
        add(BigText(claim.province.name))
        add(new StateComponentColorName(to))
      })
      case Claim.VassalizationClaim(state, possibleVassal, claimTurnEnd) => Some(new MigPane {
        add(new StateComponentColorName(from))
        add(BigText(Localization("claims.drop.vassalization.render")))
        add(new StateComponentColorName(to))
      })
    }

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class BreakFriendshipTreaty(val from: State, val to: State) extends DiplomaticDeclaration {

    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      super.beforeSendAction(diplomacy, currentTurn)
      diplomacy.agreementsAnd(from, to).collectFirst {
        case agreement: FriendshipAgreement => agreement
      }.foreach { ag =>
        diplomacy.cancelAgreement(from, ag, currentTurn)
        diplomacy.increaseBadBoy(from, CancellingFriendshipBadBoy)
      }
    }

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      diplomacy.agreementsAnd(from, to).collectFirst {
        case agreement: FriendshipAgreement => agreement
      }.nonEmpty
    }

    override def messageTitle: String = Localization("diplomacy.breakFriendship.title", from.name)

    override def body: String = Localization("diplomacy.breakFriendship.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("diplomacy.breakFriendship.report")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }

  class BreakAllianceTreaty(val from: State, val to: State) extends DiplomaticDeclaration {

    override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
      super.beforeSendAction(diplomacy, currentTurn)
      diplomacy.agreementsAnd(from, to).collectFirst {
        case agreement: AllianceAgreement => agreement
      }.foreach { ag =>
        diplomacy.cancelAgreement(from, ag, currentTurn)
        diplomacy.increaseBadBoy(from, LeavingAllianceBadBoy)
      }
    }

    override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

    override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
      diplomacy.agreementsAnd(from, to).collectFirst {
        case agreement: AllianceAgreement => agreement
      }.nonEmpty
    }

    override def messageTitle: String = Localization("diplomacy.breakAlliance.title", from.name)

    override def body: String = Localization("diplomacy.breakAlliance.body", from.name)

    override def renderInReport: Option[Node] = Some(new MigPane {
      add(new StateComponentColorName(from))
      add(BigText(Localization("diplomacy.breakAlliance.report")))
      add(new StateComponentColorName(to))
    })

    override def shouldRefreshMapAfterAnswer: Boolean = false
  }
}

class ReleaseVassal(val from: State, val to: State) extends DiplomaticDeclaration {

  override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
    super.beforeSendAction(diplomacy, currentTurn)

    diplomacy.agreementsAnd(from, to).collectFirst {
      case a: VassalAgreement if a.overlord == from && a.vassal == to => a
    }.foreach { ag =>
      diplomacy.cancelAgreement(from, ag, currentTurn)
    }
  }

  override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

  override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
    diplomacy.agreementsAnd(from, to).collectFirst {
      case a: VassalAgreement if a.overlord == from && a.vassal == to => a
    }.nonEmpty
  }

  override def messageTitle: String = Localization("diplomacy.releaseVassal.title", from.name)

  override def body: String = Localization("diplomacy.releaseVassal.body", from.name)


  override def renderInReport: Option[Node] = Some(new MigPane {
    add(new StateComponentColorName(from))
    add(BigText(Localization("diplomacy.releaseVassal.report")))
    add(new StateComponentColorName(to))
  })

  override def shouldRefreshMapAfterAnswer: Boolean = false
}

class StopBeingVassal(val from: State, val to: State) extends DiplomaticDeclaration {

  override def beforeSendAction(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {
    super.beforeSendAction(diplomacy, currentTurn)

    diplomacy.agreementsAnd(from, to).collectFirst {
      case a: VassalAgreement if a.overlord == to && a.vassal == from => a
    }.foreach { ag =>
      diplomacy.cancelAgreement(from, ag, currentTurn)
    }
    diplomacy.addClaim(VassalizationClaim(to, from, currentTurn + VassalizationClaimTurns))
  }

  override def ok(diplomacy: WorldDiplomacy, currentTurn: Int): Unit = {}

  override def isPossible(diplomacy: WorldDiplomacy, currentTurn: Int): Boolean = {
    diplomacy.agreementsAnd(from, to).collectFirst {
      case a: VassalAgreement if a.overlord == to && a.vassal == from => a
    }.nonEmpty
  }

  override def messageTitle: String = Localization("diplomacy.stopBeingVassal.title", from.name)

  override def body: String = Localization("diplomacy.stopBeingVassal.body", from.name)

  override def renderInReport: Option[Node] = Some(new MigPane {
    add(new StateComponentColorName(from))
    add(BigText(Localization("diplomacy.stopBeingVassal.report")))
    add(new StateComponentColorName(to))
  })

  override def shouldRefreshMapAfterAnswer: Boolean = false
}