package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.TakeMoney
import mr.merc.diplomacy.DiplomaticAgreement.{VassalAgreement, WarAgreement}
import mr.merc.diplomacy.DiplomaticMessage._

class WarTest extends AbstractDiplomacyTest {

  test("simple declare war") {
    val List(first, second, third) = states

    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set())
    actions.sendMessage(declareWar)

    actions.mailbox(second) shouldBe List(declareWar)

    val List(wa) = actions.agreements(first)

    actions.agreements(second) shouldBe List(wa)

    actions.agreements(third) shouldBe Nil

    actions.defaultCustomMessageAnswer(declareWar)

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil
  }

  test("vassal joined automatically when defend") {
    val List(first, second, third) = states

    sendAndAccept(new VassalizationProposal(first, third))

    actions.processUnansweredMessages()

    val declareWar = new DeclareWar(second, first, TakeMoney(second, first), Set())
    actions.sendMessage(declareWar)

    actions.mailbox(first) shouldBe List(declareWar)
    actions.answerDeclareWar(declareWar, Set(third))

    val List(order) = actions.mailbox(third)
    val orderJoinWar = order.asInstanceOf[OrderJoinWar]

    actions.acknowledgeMessage(orderJoinWar)

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil

    val List(wa) = actions.agreements(second)

    actions.agreements(first) should contain (wa)
    actions.agreements(third) should contain (wa)

    val wag = wa.asInstanceOf[WarAgreement]
    wag.sides shouldBe states.toSet

    wag.attackers shouldBe Set(second)
    wag.defenders shouldBe Set(first, third)

    actions.attackerLeader(wag) shouldBe second
    actions.defendersLeader(wag) shouldBe first
  }

  test("vassal joined automatically when asked") {
    val List(first, second, third) = states

    sendAndAccept(new VassalizationProposal(first, third))

    actions.processUnansweredMessages()

    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set(third))
    actions.sendMessage(declareWar)

    val List(order) = actions.mailbox(third)
    val orderJoinWar = order.asInstanceOf[OrderJoinWar]

    actions.acknowledgeMessage(orderJoinWar)

    actions.mailbox(second) shouldBe List(declareWar)
    actions.answerDeclareWar(declareWar, Set())

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil

    val List(wa) = actions.agreements(second)

    actions.agreements(first) should contain (wa)
    actions.agreements(third) should contain (wa)

    val wag = wa.asInstanceOf[WarAgreement]
    wag.sides shouldBe states.toSet

    wag.attackers shouldBe Set(first, third)
    wag.defenders shouldBe Set(second)

    actions.attackerLeader(wag) shouldBe first
    actions.defendersLeader(wag) shouldBe second
  }

  test("alliance member asked to help and joined war") {
    val List(first, second, third) = states

    sendAndAccept(new AllianceProposal(first, third))

    actions.processUnansweredMessages()

    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set(third))
    actions.sendMessage(declareWar)

    val List(order) = actions.mailbox(third)
    val askJoinWar = order.asInstanceOf[AskJoinWar]

    actions.answerMessage(askJoinWar, true)

    actions.mailbox(second) shouldBe List(declareWar)
    actions.answerDeclareWar(declareWar, Set())

    val List(allyAccepted) = actions.mailbox(first)
    val agreed = allyAccepted.asInstanceOf[AgreeJoinWar]
    agreed.from shouldBe third
    agreed.to shouldBe first

    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil

    val List(wa) = actions.agreements(second)

    actions.agreements(first) should contain (wa)
    actions.agreements(third) should contain (wa)

    val wag = wa.asInstanceOf[WarAgreement]
    wag.sides shouldBe states.toSet

    wag.attackers shouldBe Set(first, third)
    wag.defenders shouldBe Set(second)

    actions.attackerLeader(wag) shouldBe first
    actions.defendersLeader(wag) shouldBe second
  }

  test("alliance member asked to help and refused") {
    val List(first, second, third) = states

    sendAndAccept(new AllianceProposal(first, third))

    actions.processUnansweredMessages()

    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set(third))
    actions.sendMessage(declareWar)

    val List(order) = actions.mailbox(third)
    val askJoinWar = order.asInstanceOf[AskJoinWar]

    actions.answerMessage(askJoinWar, false)

    actions.mailbox(second) shouldBe List(declareWar)
    actions.answerDeclareWar(declareWar, Set())

    val List(allyAccepted) = actions.mailbox(first)
    val declined = allyAccepted.asInstanceOf[DeclineJoinWar]
    declined.from shouldBe third
    declined.to shouldBe first

    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil

    val List(wa) = actions.agreements(second)

    actions.agreements(first) should contain (wa)
    actions.agreements(third) shouldBe Nil

    val wag = wa.asInstanceOf[WarAgreement]
    wag.sides shouldBe Set(first, second)

    wag.attackers shouldBe Set(first)
    wag.defenders shouldBe Set(second)

    actions.attackerLeader(wag) shouldBe first
    actions.defendersLeader(wag) shouldBe second
  }

  test("vassal attacked and overlord joined war"){
    val List(first, second, third) = states

    sendAndAccept(new OverlordshipProposal(third, first))

    actions.processUnansweredMessages()

    val vassalAgreement = actions.agreements(first).head.asInstanceOf[VassalAgreement]
    vassalAgreement.overlord shouldBe first
    vassalAgreement.vassal shouldBe third

    val declareWar = new DeclareWar(second, third, TakeMoney(second, third), Set())
    actions.sendMessage(declareWar)

    actions.mailbox(third) shouldBe List(declareWar)
    actions.defaultCustomMessageAnswer(declareWar)

    val List(order) = actions.mailbox(first)
    val orderJoinWar = order.asInstanceOf[OrderJoinWar]

    actions.acknowledgeMessage(orderJoinWar)

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil

    val List(wa) = actions.agreements(second)

    actions.agreements(first) should contain (wa)
    actions.agreements(third) should contain (wa)

    val wag = wa.asInstanceOf[WarAgreement]
    wag.sides shouldBe states.toSet

    wag.attackers shouldBe Set(second)
    wag.defenders shouldBe Set(first, third)

    actions.attackerLeader(wag) shouldBe second
    actions.defendersLeader(wag) shouldBe first
  }

  test("vassal can't declare war") {
    val List(first, second, third) = states

    sendAndAccept(new VassalizationProposal(first, third))

    actions.processUnansweredMessages()

    val declareWar = new DeclareWar(third, second, TakeMoney(third, second), Set(third))
    actions.sendMessage(declareWar)

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
    actions.mailbox(third) shouldBe Nil
  }

  test("complicated alliance situation 1") {
    val List(first, second, third) = states

    sendAndAccept(new AllianceProposal(first, third))
    sendAndAccept(new AllianceProposal(second, third))

    actions.processUnansweredMessages()
    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set(third))
    actions.sendMessage(declareWar)

    actions.diplomacyEngine.areAllies(first, third) shouldBe true
    actions.diplomacyEngine.areAllies(second, third) shouldBe true

    actions.answerDeclareWar(declareWar, Set(third))

    actions.diplomacyEngine.areAllies(first, third) shouldBe true
    actions.diplomacyEngine.areAllies(second, third) shouldBe true

    val List(war) = actions.diplomacyEngine.wars
    war.sides shouldBe Set(first, second)

    val messages = actions.mailbox(third)
    messages.size shouldBe 2
    val askJoinWar1 = messages.find(_.from == first).get.asInstanceOf[AskJoinWar]
    val askJoinWar2 = messages.find(_.from == second).get.asInstanceOf[AskJoinWar]

    actions.answerMessage(askJoinWar1, true)

    war.sides shouldBe Set(first, second, third)
    war.attackers shouldBe Set(first, third)
    war.attackersLeader(actions.diplomacyEngine) shouldBe first
    war.defenders shouldBe Set(second)

    actions.mailbox(third).size shouldBe 0
  }

  test("complicated alliance situation 2") {
    val List(first, second, third) = states

    sendAndAccept(new AllianceProposal(first, third))
    sendAndAccept(new AllianceProposal(second, third))

    actions.processUnansweredMessages()
    val declareWar = new DeclareWar(first, second, TakeMoney(first, second), Set(third))
    actions.sendMessage(declareWar)

    actions.diplomacyEngine.areAllies(first, third) shouldBe true
    actions.diplomacyEngine.areAllies(second, third) shouldBe true

    actions.answerDeclareWar(declareWar, Set(third))

    actions.diplomacyEngine.areAllies(first, third) shouldBe true
    actions.diplomacyEngine.areAllies(second, third) shouldBe true

    val List(war) = actions.diplomacyEngine.wars
    war.sides shouldBe Set(first, second)

    val messages = actions.mailbox(third)
    messages.size shouldBe 2
    val askJoinWar1 = messages.find(_.from == first).get.asInstanceOf[AskJoinWar]
    val askJoinWar2 = messages.find(_.from == second).get.asInstanceOf[AskJoinWar]

    actions.answerMessage(askJoinWar2, true)

    war.sides shouldBe Set(first, second, third)
    war.attackers shouldBe Set(first)
    war.defendersLeader(actions.diplomacyEngine) shouldBe second
    war.defenders shouldBe Set(second, third)

    actions.mailbox(third).size shouldBe 0
  }
}
