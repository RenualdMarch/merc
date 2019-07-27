package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticMessage.{AllianceProposal, VassalizationProposal}

class InvalidMessageTest extends AbstractDiplomacyTest {

  test("vassalization offer when in alliance") {
    val List(first, second, third) = states

    val ap = new AllianceProposal(first, second)
    actions.sendMessage(ap)
    actions.answerMessage(ap, true)

    actions.processUnansweredMessages()

    val vp = new VassalizationProposal(first, second)
    actions.sendMessage(vp)

    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe Nil
  }
}