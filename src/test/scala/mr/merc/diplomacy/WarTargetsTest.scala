package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.{AllianceAgreement, VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.{CrackState, TakeProvince, Vassalize}
import mr.merc.diplomacy.DiplomaticMessage.{AcceptedPeaceProposal, AskJoinWar, DeclareWar, ProposePeace, ProposeSeparatePeace}
import mr.merc.diplomacy.RelationshipEvent.SeparatePeace

class WarTargetsTest extends AbstractDiplomacyTest {

  override def statesCount = 3

  test("war with white peace") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set()))
    actions.processUnansweredMessages()

    secondProvince1.controller = first

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]

    sendAndAccept(new ProposePeace(first, second, agreement, Set()))

    secondProvince1.controller shouldBe second
    secondProvince2.controller shouldBe second
    firstProvince1.controller shouldBe first
    firstProvince2.controller shouldBe first

    secondProvince1.owner shouldBe second
    secondProvince2.owner shouldBe second
    firstProvince1.owner shouldBe first
    firstProvince2.owner shouldBe first

    val accepted = actions.mailbox(first).head.asInstanceOf[AcceptedPeaceProposal]
    accepted.from shouldBe second
    accepted.to shouldBe first
  }

  test("war with province taking") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set()))
    actions.processUnansweredMessages()

    secondProvince1.controller = first

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]

    sendAndAccept(new ProposePeace(first, second, agreement, agreement.targets))

    secondProvince1.controller shouldBe first
    secondProvince2.controller shouldBe second
    firstProvince1.controller shouldBe first
    firstProvince2.controller shouldBe first

    secondProvince1.owner shouldBe first
    secondProvince2.owner shouldBe second
    firstProvince1.owner shouldBe first
    firstProvince2.owner shouldBe first

    val accepted = actions.mailbox(first).head.asInstanceOf[AcceptedPeaceProposal]
    accepted.from shouldBe second
    accepted.to shouldBe first
  }

  test("war with cracking state") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, CrackState(first, second, false), Set()))
    actions.processUnansweredMessages()

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]
    sendAndAccept(new ProposePeace(first, second, agreement, agreement.targets))

    val newState = (actions.regions.map(_.owner).toSet -- Set(first, second)).head

    newState.primeCulture shouldBe second.primeCulture
    newState.politicalSystem.rulingParty shouldBe first.politicalSystem.rulingParty

    firstProvince1.owner shouldBe first
    firstProvince2.owner shouldBe first
    firstProvince1.controller shouldBe first
    firstProvince2.controller shouldBe first

    if (secondProvince1.owner == newState) {
      secondProvince1.controller shouldBe newState
      secondProvince2.controller shouldBe second
      secondProvince1.owner shouldBe newState
      secondProvince2.owner shouldBe second
    } else if (secondProvince2.owner == newState) {
      secondProvince1.controller shouldBe second
      secondProvince2.controller shouldBe newState
      secondProvince1.owner shouldBe second
      secondProvince2.owner shouldBe newState
    } else {
      fail("Didn't find province for newState")
    }

  }

  test("war with vassalization") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, new Vassalize(first, second), Set()))
    actions.processUnansweredMessages()

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]
    sendAndAccept(new ProposePeace(first, second, agreement, agreement.targets))

    secondProvince1.owner shouldBe second
    secondProvince2.owner shouldBe second
    firstProvince1.owner shouldBe first
    firstProvince2.owner shouldBe first

    val List(agr) = actions.agreements(first)
    val vassalAgreement = agr.asInstanceOf[VassalAgreement]
    vassalAgreement.vassal shouldBe second
    vassalAgreement.overlord shouldBe first
  }

  test("war when war target becomes not possible") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set()))
    actions.processUnansweredMessages()

    val newState = actions.generateNewState(second.primeCulture, second.rulingParty, 0)

    secondProvince1.owner = newState
    secondProvince1.controller = newState

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]
    sendAndAccept(new ProposePeace(first, second, agreement, agreement.targets))

    secondProvince1.owner shouldBe newState
    secondProvince2.owner shouldBe second
    firstProvince1.owner shouldBe first
    firstProvince2.owner shouldBe first
  }

  test("when peace proposal declined, nothing happens") {
    val List(first, second, _) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)

    actions.sendMessage(new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set()))
    actions.processUnansweredMessages()

    val agreement = actions.agreements(first).head.asInstanceOf[WarAgreement]
    sendAndDecline(new ProposePeace(first, second, agreement, agreement.targets))

    List(agreement) shouldBe actions.agreements(first)
  }

  test("separate peace test - separatist asks") {
    val List(first, second, third) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)
    val List(thirdProvince1, thirdProvince2) = actions.regions.filter(_.owner == third)

    actions.diplomacyEngine.addAgreement(new AllianceAgreement(first, third, 0))

    val dw = new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set(third))
    actions.sendMessage(dw)
    actions.answerDeclareWar(dw, Set())
    val messages = actions.diplomacyEngine.messages(third, 0)
    messages.size shouldBe 1
    val askJoinWar = messages.head.asInstanceOf[AskJoinWar]
    actions.answerMessage(askJoinWar, true)

    val List(war) = actions.diplomacyEngine.wars

    val sp = ProposeSeparatePeace(third, second, war, Set(), third)
    actions.sendMessage(sp)

    actions.mailbox(second).find(_ == sp).get
    actions.answerMessage(sp, true)
    war.sides shouldBe Set(first, second)
  }

  test("separate peace test - leader asks") {
    val List(first, second, third) = states

    val List(firstProvince1, firstProvince2) = actions.regions.filter(_.owner == first)
    val List(secondProvince1, secondProvince2) = actions.regions.filter(_.owner == second)
    val List(thirdProvince1, thirdProvince2) = actions.regions.filter(_.owner == third)

    actions.diplomacyEngine.addAgreement(new AllianceAgreement(first, third, 0))

    val dw = new DeclareWar(first, second, new TakeProvince(first, second, secondProvince1), Set(third))
    actions.sendMessage(dw)
    actions.answerDeclareWar(dw, Set())
    val messages = actions.diplomacyEngine.messages(third, 0)
    messages.size shouldBe 1
    val askJoinWar = messages.head.asInstanceOf[AskJoinWar]
    actions.answerMessage(askJoinWar, true)

    val List(war) = actions.diplomacyEngine.wars

    val sp = ProposeSeparatePeace(second, third, war, Set(), third)
    actions.sendMessage(sp)

    actions.mailbox(third).find(_ == sp).get
    actions.answerMessage(sp, true)
    war.sides shouldBe Set(first, second)
  }
}
