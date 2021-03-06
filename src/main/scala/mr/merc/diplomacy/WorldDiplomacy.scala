package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, FriendshipAgreement, SanctionAgreement, TruceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement._
import mr.merc.diplomacy.Claim.{ProvinceClaim, StrongProvinceClaim, VassalizationClaim, WeakProvinceClaim}
import mr.merc.diplomacy.DiplomaticMessage.DeclareWar
import mr.merc.diplomacy.WorldDiplomacy.RelationshipBonus
import mr.merc.economics.Culture.CultureAlignment.{ColorAlignment, PriorityAlignment}
import mr.merc.politics.{ForeignPolicy, Province, State}
import mr.merc.economics.WorldConstants.Diplomacy._
import mr.merc.economics.message.InformationDomesticMessage
import mr.merc.economics.{Culture, WorldConstants, WorldStateDiplomacyActions}
import mr.merc.local.Localization
import mr.merc.ui.world.ClaimReceivedDomesticMessagePane
import scalafx.scene.layout.Region
import scalafx.Includes._

import scala.annotation.tailrec
import scala.util.Random


class WorldDiplomacy(actions: WorldStateDiplomacyActions) {
  def regions: List[Province] = actions.regions

  private var currentTurnMessages: List[DiplomaticMessage] = Nil

  def turnMessagesReport: List[DiplomaticMessage] = currentTurnMessages.reverse

  private var savedTurn = -1

  def currentTurn: Int = actions.turn

  def states: Set[State] = regions.toSet.map { p: Province => p.owner }

  private var agreements: List[DiplomaticAgreement] = Nil
  private var events: List[RelationshipEvent] = Nil

  private var _badBoy: Map[State, Double] = Map()

  def badBoy: Map[State, Double] = _badBoy

  private var mailbox: Map[State, List[DiplomaticMessage]] = Map()

  def distancesToAllOtherReachableStates(state: State, maxDistance: Option[Int]): Map[State, Int] = {
    val streams = actions.states(state).map { province =>
      maxDistance match {
        case Some(value) => neighboursStream(province).takeWhile(_._1 <= value)
        case None => neighboursStream(province)
      }
    }
    streams.flatten.filterNot(_._2.owner == state).map { case (d, p) =>
      p.owner -> d
    }.groupBy(_._1).map { case (k, v) =>
      k -> v.map(_._2).min
    }
  }

  def distanceFromStateToState(from: State, to: State): Option[Int] = {
    actions.states(from).flatMap { province =>
      neighboursStream(province).find(_._2.owner == to).map(_._1)
    } match {
      case Nil => None
      case list => Some(list.min)
    }
  }

  private def neighboursStream(startingProvince: Province): Stream[(Int, Province)] = {

    def nextNeighbours(currentCircle: Set[Province], alreadySent: Set[Province]): Set[Province] = {
      val present = currentCircle & alreadySent
      currentCircle.flatMap(_.neighbours) -- present
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case None => Stream.Empty
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      }
    }

    case class StreamState(step: Int, circle: Set[Province], sent: Set[Province])

    unfold[Set[(Int, Province)], StreamState](StreamState(0, Set(startingProvince), Set())) { case StreamState(step, circle, sent) =>
      val newCircle = nextNeighbours(circle, sent)
      if (newCircle.nonEmpty) {
        val stream = newCircle.map(c => (step + 1) -> c)
        val ss = StreamState(step + 1, newCircle, circle & sent)
        Some((stream, ss))
      } else {
        None
      }
    }.flatten
  }

  private class ClaimsHolder {
    private var _claims: Set[Claim] = Set()

    def claims: Set[Claim] = _claims

    def containsAnyClaim(state: State, province: Province): Boolean = {
      containsStrongClaim(state, province) || containsWeakClaim(state, province)
    }

    def containsStrongClaim(state: State, province: Province): Boolean = {
      _claims.contains(StrongProvinceClaim(state, province))
    }

    def findWeakClaim(state: State, province: Province): Option[WeakProvinceClaim] = {
      _claims.collectFirst {
        case wc@WeakProvinceClaim(owner, land, _) if owner == state && province == land => wc
      }
    }

    def containsWeakClaim(state: State, province: Province): Boolean = {
      findWeakClaim(state, province).nonEmpty
    }

    def containsVassalizationClaim(state: State, possibleVassal: State): Boolean = {
      _claims.collectFirst {
        case VassalizationClaim(lord, vassal, _) => lord == state && vassal == possibleVassal
      }.nonEmpty
    }

    def safelyAddClaim(claim: Claim): Boolean = {
      claim match {
        case wc: WeakProvinceClaim =>
          if (!containsAnyClaim(wc.state, wc.province)) {
            _claims += wc
            true
          } else false
        case sc: StrongProvinceClaim =>
          if (!containsStrongClaim(sc.state, sc.province)) {
            findWeakClaim(sc.state, sc.province) match {
              case Some(wc) =>
                _claims -= wc
                _claims += sc
              case None =>
                _claims += sc
            }
            true
          } else false
        case vc: VassalizationClaim =>
          if (!containsVassalizationClaim(vc.state, vc.possibleVassal)) {
            _claims += vc
            true
          } else false
      }
    }

    def cleanClaims(actualStates: Set[State]): Unit = {
      _claims = _claims.filter {
        case claim: ProvinceClaim => actualStates.contains(claim.state)
        case VassalizationClaim(state, possibleVassal, claimTurnEnd) =>
          actualStates.contains(state) && actualStates.contains(possibleVassal)
      }
    }

    def dropClaims(claims: Traversable[Claim]): Unit = {
      _claims --= claims
    }

    def dropClaim(claim: Claim): Unit = {
      dropClaims(List(claim))
    }
  }

  private val claimsHolder = new ClaimsHolder()

  def allClaims: Set[Claim] = claimsHolder.claims

  def generateInitialStrongClaimsForOwnedTerritories(): Unit = {
    require(claimsHolder.claims.isEmpty, s"Claims is not empty: ${claimsHolder.claims}")
    regions.map(p => StrongProvinceClaim(p.owner, p)).foreach { c =>
      claimsHolder.safelyAddClaim(c)
    }
  }

  def generateInitialClaimsForNeighbours(): Unit = {
    generateClaimsForNeighbours(WorldConstants.Diplomacy.ChanceForInitialWeakClaim,
      WorldConstants.Diplomacy.WeakClaimTime)
  }

  def generateEndTurnClaimsForNeighbours(currentTurn: Int): Unit = {
    generateClaimsForNeighbours(WorldConstants.Diplomacy.ChanceForWeakClaim,
      currentTurn + WorldConstants.Diplomacy.WeakClaimTime)
    generateWeakClaimsForOwnedTerritoriesWithoutClaims(WorldConstants.Diplomacy.ChanceForWeakClaim,
      currentTurn + WorldConstants.Diplomacy.WeakClaimTime)
  }

  private def generateClaimsForNeighbours(percentage: Double, claimEnd: Int): Unit = {
    for {
      p <- regions if p.owner.politicalSystem.rulingParty.foreignPolicy == ForeignPolicy.Expansionism
      neig <- p.neighbours if neig.owner != p.owner && !areAllies(neig.owner, p.owner) && !areFriends(neig.owner, p.owner)
    } {
      if (Random.nextDouble() < percentage) {
        val claim = WeakProvinceClaim(p.owner, neig, claimEnd)
        val added = claimsHolder.safelyAddClaim(claim)
        if (added) {
          p.owner.mailBox.addMessage(new InformationDomesticMessage(p.owner.elites.foreignMinister, Localization("messages.claims.title")) {
            override def body: Region = new ClaimReceivedDomesticMessagePane(p.owner, claim)
          })

          claim.province.owner.mailBox.addMessage(new InformationDomesticMessage(claim.province.owner.elites.foreignMinister, Localization("messages.claims.title")) {
            override def body: Region = new ClaimReceivedDomesticMessagePane(claim.province.owner, claim)
          })

          increaseBadBoy(p.owner, ClaimCreationBadBoy)
        }
      }
    }
  }

  def generateWeakClaimsForOwnedTerritoriesWithoutClaims(percentage: Double, claimEnd: Int): Unit = {
    for {
      p <- regions if !claimsHolder.containsStrongClaim(p.owner, p)
    } {
      if (Random.nextDouble() < percentage) {
        val claim = WeakProvinceClaim(p.owner, p, claimEnd)
        val added = claimsHolder.safelyAddClaim(claim)
        if (added) {
          p.owner.mailBox.addMessage(new InformationDomesticMessage(p.owner.elites.foreignMinister, Localization("messages.claims.title")) {
            override def body: Region = new ClaimReceivedDomesticMessagePane(p.owner, claim)
          })
        }
      }
    }
  }

  def replaceWeakClaimsWithStrongClaimsForOwnedTerritories(currentTurn: Int): Unit = {
    claimsHolder.claims.collect {
      case wc: WeakProvinceClaim => wc
    }.filter(wc => wc.province.owner == wc.state).foreach { wc =>
      if (wc.claimTurnEnd == currentTurn + 1) {
        val claim = StrongProvinceClaim(wc.state, wc.province)
        claimsHolder.safelyAddClaim(claim)
        wc.state.mailBox.addMessage(new InformationDomesticMessage(wc.state.elites.foreignMinister, Localization("messages.claims.title")) {
          override def body: Region = new ClaimReceivedDomesticMessagePane(wc.state, claim)
        })
      }
    }
  }

  def dropAllClaimsAgainst(whoDrops: State, againstWho: State): Unit = {
    this.claimsHolder.dropClaims(this.claimsHolder.claims.filter(c => c.state == whoDrops && c.targetState == againstWho))
  }

  def dropClaim(claim: Claim): Unit = {
    decreaseBadBoy(claim.state, ClaimDroppingBadBoy)
    this.claimsHolder.dropClaim(claim)
  }

  def hasClaimOverProvince(state: State, province: Province): Boolean = {
    claims(state).collect {
      case w: WeakProvinceClaim => w.province
      case s: StrongProvinceClaim => s.province
    }.toSet.contains(province)
  }

  def hasVassalizationClaimOverState(state: State, targetState: State): Boolean = {
    val set = claims(state).collect {
      case v: VassalizationClaim => v.targetState
    }.toSet
    set.contains(targetState)
  }

  def addEvent(event: RelationshipEvent): Unit = {
    events ::= event
  }

  private var prevWarNames: List[String] = Nil

  def generateNewWarName(attacker: State, defender: State, target: WarTarget): String = {
    val newWarName = target match {
      case takeProvince: TakeProvince =>
        if (allClaims.collectFirst {
          case sc@StrongProvinceClaim(state, province) if state == attacker && province == takeProvince.province => sc
        } nonEmpty) {
          Localization("diplomacy.warName.liberation", takeProvince.demander.initialName, takeProvince.giver.initialName, takeProvince.province.name)
        } else {
          Localization("diplomacy.warName.conquest", takeProvince.demander.initialName, takeProvince.giver.initialName, takeProvince.province.name)
        }
      case culture: LiberateCulture =>
        Localization("diplomacy.warName.cultureLiberation", culture.demander.initialName,
          Localization(culture.culture.cultureNameKey), culture.giver.initialName)
      case money: TakeMoney =>
        Localization("diplomacy.warName.demandMoney", money.demander.initialName, money.giver.initialName)
      case vassalize: Vassalize =>
        Localization("diplomacy.warName.vassalization", vassalize.demander.initialName, vassalize.giver.initialName)
    }

    val count = prevWarNames.count(_ == newWarName)
    prevWarNames ::= newWarName
    if (count == 0) newWarName
    else s"${count + 1} $newWarName"
  }

  def improveBadBoyOverTime(): Unit = {
    _badBoy.keys.foreach(s => decreaseBadBoy(s, BadBoyTurnRecovery))
  }

  def increaseBadBoy(state: State, value: Double): Unit = {
    val current = _badBoy.getOrElse(state, 0d) + value
    _badBoy = _badBoy + (state -> current)
  }

  def decreaseBadBoy(state: State, value: Double): Unit = {
    val current = _badBoy.getOrElse(state, 0d) - value
    _badBoy = _badBoy + (state -> (if (current < 0) 0 else current))
  }

  def addAgreement(agreement: DiplomaticAgreement): Unit = {
    val invalidAgreementsMap = agreements.groupBy(a => agreement.isPreviousAgreementBroken(a, this))
    invalidAgreementsMap.collect { case (Some(k), x) => k -> x }.foreach {
      case (breakingReason, brokenAgreements) =>
        if (breakingReason.isVoluntary) {
          breakingReason.breakers.foreach { breaker =>
            events :::= brokenAgreements.flatMap(_.voluntaryBreakingAgreementEvent(breaker, agreement.signingTurn))
          }
        }
    }

    agreements = agreement :: invalidAgreementsMap.getOrElse(None, Nil)
  }

  def sendMessage(message: DiplomaticMessage, currentTurn: Int): Unit = {
    message.beforeSendAction(this, currentTurn)
    val messages = mailbox.getOrElse(message.to, Nil)
    mailbox += message.to -> (message :: messages)

    if (currentTurn != savedTurn) {
      if (WorldConstants.Diplomacy.DeleteLastTurnEvents) {
        currentTurnMessages = Nil
      }

      savedTurn = currentTurn
    }
    currentTurnMessages ::= message
  }

  def existsMessages(states: Set[State]): Boolean = {
    states.forall(s => mailbox.get(s).forall(_.nonEmpty))
  }

  def messages(state: State, currentTurn: Int): List[DiplomaticMessage] = mailbox.getOrElse(state, Nil).
    filter(_.isPossible(this, currentTurn))

  def answerMessage(question: DiplomaticProposal, answerIsYes: Boolean, currentTurn: Int): Unit = {
    extractMessage(question, currentTurn).foreach { q =>
      if (answerIsYes) {
        q.accept(this, currentTurn)
      } else {
        q.decline(this, currentTurn)
      }
    }
  }

  private def extractMessage[T <: DiplomaticMessage](message: T, currentTurn: Int): Option[T] = {
    val (foundMessage, newMessages) = messages(message.to, currentTurn).partition(_ == message)
    mailbox += message.to -> newMessages
    foundMessage.headOption.asInstanceOf[Option[T]]
  }

  def answerMessage(declaration: DiplomaticDeclaration, currentTurn: Int): Unit = {
    extractMessage(declaration, currentTurn).foreach { m =>
      m.ok(this, currentTurn)
    }
  }

  def defaultAnswerMessage(customMessage: CustomDiplomaticQuestion, currentTurn: Int): Unit = {
    extractMessage(customMessage, currentTurn).foreach { cm =>
      cm.defaultOk(this, currentTurn)
    }
  }

  def answerDeclareWarMessage(war: DeclareWar, currentTurn: Int, allies: Set[State]): Unit = {
    extractMessage(war, currentTurn).foreach { w =>
      w.okAndCallAllies(this, currentTurn, allies)
    }
  }

  def processUnansweredMessages(state: State, currentTurn: Int): Unit = {
    messages(state, currentTurn).foreach {
      case dp: DiplomaticProposal => answerMessage(dp, false, currentTurn)
      case dd: DiplomaticDeclaration => answerMessage(dd, currentTurn)
      case c: CustomDiplomaticQuestion =>
        extractMessage(c, currentTurn).foreach { m =>
          m.defaultOk(this, currentTurn)
        }
    }
  }

  def processAllUnansweredMessages(currentTurn: Int): Unit = {
    mailbox.keySet.foreach {
      processUnansweredMessages(_, currentTurn)
    }

    mailbox = mailbox.transform { case (_, v) =>
      v.filter(_.isPossible(this, currentTurn))
    }
  }

  def agreementsAnd(sides: State*): List[DiplomaticAgreement] = {
    val set = sides.toSet
    agreements.filter(a => set.subsetOf(a.sides))
  }

  def agreementsOr(sides: State*): List[DiplomaticAgreement] = {
    val set = sides.toSet
    agreements.filter(a => set.intersect(a.sides).nonEmpty)
  }

  def areAllies(first: State, second: State): Boolean = {
    agreementsAnd(first, second).collectFirst {
      case aa: AllianceAgreement if aa.sides == Set(first, second) => aa
    }.nonEmpty
  }

  def areFriends(first: State, second: State): Boolean = {
    agreementsAnd(first, second).collectFirst {
      case fa: FriendshipAgreement if fa.sides == Set(first, second) => fa
    }.nonEmpty
  }

  def isVassal(overlord: State, vassal: State): Boolean = {
    agreementsAnd(overlord, vassal).collectFirst {
      case va: VassalAgreement if va.overlord == overlord && va.vassal == vassal => va
    } nonEmpty
  }

  def getOverlord(vassal: State): Option[State] = {
    agreementsAnd(vassal).collect {
      case va: VassalAgreement if va.vassal == vassal => va
    }.map(_.overlord).headOption
  }

  def getVassals(overlord: State): List[State] = {
    agreementsAnd(overlord).collect {
      case va: VassalAgreement if va.overlord == overlord => va
    }.map(_.vassal)
  }

  def events(from: State): List[RelationshipEvent] =
    events.filter(_.fromState == from)

  def events(from: State, to: State): List[RelationshipEvent] =
    events.filter(e => e.fromState == from && e.toState == to)

  def claimsBonuses(from: State): List[RelationshipBonus] = {
    claimsHolder.claims.filter(s => (s.state == from || s.targetState == from) && (s.state != s.targetState)).map {
      case vc: VassalizationClaim if vc.possibleVassal == from =>
        RelationshipBonus(vc.possibleVassal, vc.state, VassalizationClaimsOnUsRelationshipChange,
          Localization("diplomacy.hasVassalizationClaim", vc.state.name, vc.possibleVassal.name))
      case vc: VassalizationClaim =>
        RelationshipBonus(vc.state, vc.possibleVassal, VassalizationClaimsOnThemRelationshipChange,
          Localization("diplomacy.hasVassalizationClaim", vc.state.name, vc.possibleVassal.name))
      case str: ProvinceClaim if str.state == from =>
        RelationshipBonus(str.state, str.targetState, ClaimsOnUsRelationshipChange,
          Localization("diplomacy.hasClaim", str.state.name, str.province.name, str.targetState.name))
      case str: ProvinceClaim =>
        RelationshipBonus(str.targetState, str.state, ClaimsOnThemRelationshipChange,
          Localization("diplomacy.hasClaim", str.state.name, str.province.name, str.targetState.name))
    }.toList
  }

  def rivalOfRivalBonus(from: State): List[RelationshipBonus] = {
    val enemies = actions.situation.rivals(from)
    val exclusionSet = enemies.toSet + from
    val rivalsOfRivals = enemies.flatMap(e => actions.situation.rivals(e).map(e -> _))
    rivalsOfRivals.filterNot{case (_, ee) => exclusionSet.contains(ee)}.map { case (rival, rivalOfRival) =>
      RelationshipBonus(from, rivalOfRival, RivalOfRivalRelationshipBonus,
        Localization("diplomacy.rivalOfRival", rivalOfRival.name, from.name, rival.name))
    }
  }

  def raceAndCultureBonuses(from: State, to: State): List[RelationshipBonus] = {
    val fromAlignment = from.primeCulture.cultureAlignment
    val toAlignment = to.primeCulture.cultureAlignment

    def localizationR(a: ColorAlignment, b: ColorAlignment) = {
      val key = if (a == b) "diplomacy.religionAlignment.same" else "diplomacy.religionAlignment.different"
      Localization(key, from.name, a.localize, to.name, b.localize)
    }

    def localizationP(a: PriorityAlignment, b: PriorityAlignment) = {
      val key = if (a == b) "diplomacy.priorityAlignment.same" else "diplomacy.priorityAlignment.different"
      Localization(key, from.name, a.localize, to.name, b.localize)
    }

    val religionBonus = (fromAlignment.religionAlignment, toAlignment.religionAlignment) match {
      case (a, b) if a == b =>
        RelationshipBonus(from, to, SameReligionRelationshipBonus, localizationR(a, b))
      case (a, b) if Set(ColorAlignment.Dark, ColorAlignment.Light) == Set(a, b) =>
        RelationshipBonus(from, to, OppositeReligionRelationshipBonus, localizationR(a, b))
      case (a, b) =>
        RelationshipBonus(from, to, NeighbourReligionRelationshipBonus, localizationR(a, b))
    }

    val priorityBonus = (fromAlignment.priorityAlignment, toAlignment.priorityAlignment) match {
      case (a, b) if a == b =>
        RelationshipBonus(from, to, SamePriorityRelationshipBonus, localizationP(a, b))
      case (a, b) if Set(PriorityAlignment.Intellectual, PriorityAlignment.Physical) == Set(a, b) =>
        RelationshipBonus(from, to, OppositePriorityRelationshipBonus, localizationP(a, b))
      case (a, b) =>
        RelationshipBonus(from, to, NeighbourPriorityRelationshipBonus, localizationP(a, b))
    }

    List(religionBonus, priorityBonus)
  }

  def clearExpiredEvents(currentTurn: Int): Unit = {
    events = events.filter(_.isDurationEnded(currentTurn))
  }

  def relationships(state: State, currentTurn: Int): Map[State, Int] = {
    relationshipsDescribed(state, currentTurn).transform { case (_, list) =>
      list.map(_.bonus).sum
    }.withDefaultValue(0)
  }

  def relationshipsDescribed(state: State, currentTurn: Int): Map[State, List[RelationshipBonus]] = {
    val bonuses = events(state).map(_.relationshipsChange(currentTurn)) ++ neighboursBonuses(state) ++
      agreementsAnd(state).flatMap(_.relationshipBonus).filter(_.from == state) ++ rivalOfRivalBonus(state) ++
      claimsBonuses(state) ++ reputationBonuses(state) ++ (states - state).flatMap(s => raceAndCultureBonuses(state, s))
    bonuses.groupBy(_.to)
  }

  def reputationBonuses(state: State): List[RelationshipBonus] = {
    val maxDistance = WorldConstants.Diplomacy.BadBoyToRelationshipConversion.size
    distancesToAllOtherReachableStates(state, Some(maxDistance)).flatMap { case (to, dist) =>
      badBoy.get(to).filterNot(_ == 0).map { bb =>
        val result = bb * WorldConstants.Diplomacy.BadBoyToRelationshipConversion(dist - 1)
        RelationshipBonus(state, to, result.toInt,
          Localization("diplomacy.reputation.relationshipBonus", to.name))
      }
    }.toList
  }

  def neighboursBonuses(state: State): List[RelationshipBonus] = {
    val neighbours = regions.filter(_.owner == state).flatMap { p =>
      p.neighbours.map(_.owner)
    }.toSet - state

    neighbours.filterNot { n =>
      claimsHolder.claims.exists(c => Set(c.targetState, c.state) == Set(state, n))
    }.map { n =>
      RelationshipBonus(state, n, NeighboursWithoutClaimsRelationshipBonus,
        Localization("diplomacy.neigsAtPeace", state.name, n.name))
    }.toList
  }

  def wars: List[WarAgreement] = agreements.collect {
    case wa: WarAgreement => wa
  }

  def wars(state: State): List[WarAgreement] = agreements.collect {
    case wa: WarAgreement if wa.sides.contains(state) => wa
  }

  def addClaim(claim: Claim): Unit = {
    claimsHolder.safelyAddClaim(claim)
  }

  def claims(state: State): List[Claim] = claimsHolder.claims.filter(_.state == state).toList

  def claimsAgainst(state: State): List[Claim] = claimsHolder.claims.filter(c => c.targetState == state && c.state != state).toList

  def claimsFromAgainst(from: State, to: State): List[Claim] = claimsHolder.claims.filter(c => c.targetState == to && c.state == from).toList

  def joinWar(wa: WarAgreement, ally: State, newSide: State, currentTurn: Int): Unit = {
    if (wa.defenders.contains(ally)) {
      wa.defenders += newSide
    } else if (wa.attackers.contains(ally)) {
      wa.attackers += newSide
    } else {
      sys.error(s"Can't add $newSide to war because $ally doesn't belong in war $wa")
    }
    agreements = agreements.filterNot(_ == wa)
    addAgreement(wa)
  }

  def cancelAgreement(breaker: State, a: DiplomaticAgreement, currentTurn: Int): Unit = {
    agreements = agreements.filterNot(_ == a)
    val breakingEvents = a.voluntaryBreakingAgreementEvent(breaker, currentTurn)
    events ++= breakingEvents
  }

  private def moveBackControlToOwnersAfterWar(wa: WarAgreement, separatePeace: Boolean): Unit = {
    val remainingWars = if (separatePeace) wars else wars.filterNot(_ == wa)
    regions.filter(p => wa.sides.contains(p.owner) && p.controller != p.owner).foreach { p =>
      if (!remainingWars.exists(w => w.onDifferentSides(Set(p.owner, p.controller)))) {
        p.controller = p.owner
      }
    }
  }

  def acceptPeaceTreaty(wa: WarAgreement, targets: Set[WarTarget], currentTurn: Int): Unit = {
    moveBackControlToOwnersAfterWar(wa, separatePeace = false)

    agreements = agreements.filterNot(_ == wa)
    val newAgreements = targets.flatMap(t => applyWarTarget(wa, t, currentTurn))
    newAgreements.foreach(this.addAgreement)

    removeDisappearedStates()
  }

  def acceptSeparatePeaceTreaty(warAgreement: WarAgreement, separateState: State, targets: Set[WarTarget], currentTurn: Int): Unit = {
    val separate = separateState :: getVassals(separateState)
    if (!warAgreement.removeSides(separate.toSet, this)) {
      agreements = agreements.filterNot(_ == warAgreement)
    }

    val newAgreements = targets.flatMap(t => applyWarTarget(warAgreement, t, currentTurn))
    newAgreements.foreach(this.addAgreement)

    moveBackControlToOwnersAfterWar(warAgreement, separatePeace = true)
    increaseBadBoy(separateState, WorldConstants.Diplomacy.SeparatePeaceBadBoy)
    removeDisappearedStates()
  }

  private def giveAllMoneyToAnnexor(giver: State, demander: State): Unit = {
    val money = giver.budget.moneyReserve
    giver.budget.spendMoneyOnReparations(giver.budget.moneyReserve)
    demander.budget.receiveMoneyFromReparations(money)
  }

  private def applyWarTarget(warAgreement: WarAgreement, warTarget: WarTarget, currentTurn: Int): List[DiplomaticAgreement] = {
    List(warTarget).flatMap(_.validTarget(warAgreement, this)).flatMap {
      case tp: TakeProvince =>
        if (!hasClaimOverProvince(tp.demander, tp.province)) {
          increaseBadBoy(tp.demander, AnnexedProvinceWithoutClaimBadBoy)
        }
        tp.province.owner = tp.demander
        tp.province.controller = tp.demander
        if (!regions.exists(_.owner == tp.giver)) {
          giveAllMoneyToAnnexor(tp.giver, tp.demander)
        }
        Nil
      case lc: LiberateCulture =>
        increaseBadBoy(lc.demander, LiberateCultureBadBoy)
        val newState = actions.generateNewState(lc.culture, lc.demander.rulingParty, lc.giver.technologyLevel.technologyLevel, 0)
        lc.provinces.foreach { p =>
          p.owner = newState
          p.controller = newState
        }
        if (!regions.exists(_.owner == lc.giver)) {
          giveAllMoneyToAnnexor(lc.giver, lc.demander)
        }
        Nil
      case tm: TakeMoney =>
        val money = tm.giver.budget.spendMoneyOnReparations(tm.giver.budget.moneyReserve)
        tm.demander.budget.receiveMoneyFromReparations(money)
        Nil
      case v: Vassalize =>
        if (!hasVassalizationClaimOverState(v.demander, v.giver)) {
          increaseBadBoy(v.demander, VassalizedStateBadBoy)
        }
        List(new VassalAgreement(v.demander, v.giver, currentTurn))
    }
  }

  def removeDisappearedStates(): Unit = {
    val actualStates = states

    def actual(set: Set[State]): Set[State] = set & actualStates

    val agreementsToDelete = agreements.flatMap {
      case warAgreement: WarAgreement =>
        warAgreement.attackers = actual(warAgreement.attackers)
        warAgreement.defenders = actual(warAgreement.defenders)
        warAgreement.targets = warAgreement.targets.flatMap(_.validTarget(warAgreement, this))
        if (warAgreement.attackers.isEmpty || warAgreement.defenders.isEmpty || warAgreement.targets.isEmpty)
          Some(warAgreement)
        else None
      case truceAgreement: TruceAgreement =>
        if (truceAgreement.sides != actual(truceAgreement.sides)) Some(truceAgreement)
        else None
      case vassalAgreement: VassalAgreement =>
        if (vassalAgreement.sides != actual(vassalAgreement.sides)) Some(vassalAgreement)
        else None
      case allianceAgreement: AllianceAgreement =>
        if (allianceAgreement.sides != actual(allianceAgreement.sides)) Some(allianceAgreement)
        else None
      case sanctionAgreement: SanctionAgreement =>
        if (sanctionAgreement.sides != actual(sanctionAgreement.sides)) Some(sanctionAgreement)
        else None
      case friendshipAgreement: FriendshipAgreement =>
        if (friendshipAgreement.sides != actual(friendshipAgreement.sides)) Some(friendshipAgreement)
        else None
    }.toSet

    agreements = agreements.filterNot(agreementsToDelete.contains)

    regions.foreach { p =>
      if (!actualStates.contains(p.controller)) {
        p.controller = p.owner
      }

      val lostWarriors = p.regionWarriors.allWarriors.filterNot(w => actualStates.contains(w.owner))
      p.regionWarriors.takeWarriors(lostWarriors)
    }

    events = events.filter(e => actual(Set(e.fromState, e.toState)).size == 2)

    _badBoy = _badBoy.filterKeys(actualStates.contains)

    mailbox = mailbox.filterKeys(actualStates.contains)

    claimsHolder.cleanClaims(actualStates)
  }

  def possibleVassalizationWarTargets(state: State): List[State] = {
    state :: getVassals(state)
  }

  def neighbourProvincesToTake(attacker: State, defender: State): List[Province] = {
    val attackerRegionsNeighbours = regions.filter(_.owner == attacker).flatMap(_.neighbours).toSet
    val defenderRegions = regions.filter(_.owner == defender).toSet
    (attackerRegionsNeighbours & defenderRegions).toList
  }

  def possibleCulturesToLiberate(state: State): Set[Culture] = {
    regions.filter(_.owner == state).map(_.culture).toSet - state.primeCulture
  }

  def possibleTargetsForStartingWar(attacker: State, defender: State): Set[WarTarget] = {
    possibleVassalizationWarTargets(defender).map { d =>
      Vassalize(attacker, d)
    } ++ neighbourProvincesToTake(attacker, defender).map { p =>
      TakeProvince(attacker, defender, p)
    } ++ possibleCulturesToLiberate(defender).map { c =>
      val provinces = regions.filter(_.owner == defender).filter(_.culture == c)
      LiberateCulture(attacker, defender, c, provinces.toSet)
    } ++ List(TakeMoney(attacker, defender))
  }.toSet

  def possibleWarTargets(warAgreement: WarAgreement, state: State): Set[WarTarget] = {
    val enemies = warAgreement.oppositeSideByState(state)
    val allies = warAgreement.sideByState(state)
    val possible: Set[WarTarget] = enemies.flatMap { en =>
      List(
        Vassalize(state, en),
        TakeMoney(state, en)) ++
        regions.filter(r => r.owner == en && allies.contains(r.controller)).map { r =>
          TakeProvince(state, en, r)
        } ++
        regions.filter(r => r.owner == en && r.culture != en.primeCulture).groupBy(_.culture).map {
          case (c, r) => LiberateCulture(state, en, c, r.toSet)
        }
    }

    (possible -- warAgreement.targets).filter { target =>
      WarTarget.areWarTargetsConsistent(warAgreement.targets + target)
    }
  }

  def addWarTarget(war: WarAgreement, warTarget: WarTarget): Unit = {
    if (possibleWarTargets(war, warTarget.demander).contains(warTarget)) {
      war.targets += warTarget
    }
  }


  def resolveStalledWars(): Unit = {
    wars.foreach { war =>
      val lastBattle = if (war.battles.nonEmpty) {
        war.battles.maxBy(_.turn).turn
      } else war.startingTurn

      if (actions.turn - lastBattle > WorldConstants.Diplomacy.StalledWarTimeUntilPeace) {
        resolveStalledWar(war)
      }
    }
  }

  def resolveStalledWar(war: WarAgreement): Unit = {
    war.targets.filter {
      case TakeProvince(demander, giver, province) =>
        war.sideByState(demander).contains(province.controller)
      case WarAgreement.LiberateCulture(demander, giver, culture, provinces) =>
        provinces.exists(p => war.sideByState(demander).contains(p.controller))
      case _: WarAgreement.TakeMoney => false
      case _: WarAgreement.Vassalize => false
    }.foreach {
      case tp: TakeProvince => applyWarTarget(war, tp, actions.turn)
      case LiberateCulture(demander, giver, culture, provinces) =>
        val controlled = provinces.filter(p => war.sideByState(demander).contains(p.controller))
        val newTarget = LiberateCulture(demander, giver, culture, controlled)
        applyWarTarget(war, newTarget, actions.turn)
      case _: TakeMoney => sys.error("Impossible case")
      case _: Vassalize => sys.error("Impossible case")
    }

  }

}

object WorldDiplomacy {

  case class RelationshipBonus(from: State, to: State, bonus: Int, title: String)

}

sealed trait Claim {
  def state: State

  def targetState: State
}

object Claim {

  sealed trait ProvinceClaim extends Claim {
    val state: State
    val province: Province
  }

  case class StrongProvinceClaim(state: State, province: Province) extends ProvinceClaim {
    override def targetState: State = province.owner
  }

  case class WeakProvinceClaim(state: State, province: Province, claimTurnEnd: Int) extends ProvinceClaim {
    override def targetState: State = province.owner
  }

  case class VassalizationClaim(state: State, possibleVassal: State, claimTurnEnd: Int) extends Claim {
    override def targetState: State = possibleVassal
  }

}

