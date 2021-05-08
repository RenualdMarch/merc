package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.AllianceAgreement
import mr.merc.diplomacy.DiplomaticMessage.{AllianceAccepted, AllianceProposal, AllianceRejected}
import mr.merc.economics.WorldConstants.Diplomacy._

class AllianceTest extends AbstractDiplomacyTest {

  test("simple accept alliance") {
    val List(first, second, third) = states

    println(actions.relationshipsDescribed(first)(second))

    actions.relationships(first) shouldBe Map(second -> (SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus),
      third -> (SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus))
    actions.relationships(second) shouldBe Map(
      first -> (SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus),
      third -> (SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus))

    val ap = new AllianceProposal(first, second)
    actions.sendMessage(ap)
    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe List(ap)

    actions.answerMessage(ap, true)

    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val aa = firstMail.head.asInstanceOf[AllianceAccepted]
    aa.from shouldBe second
    aa.to shouldBe first

    val firstAgreements = actions.agreements(first)
    firstAgreements should have size 1
    val secondAgreements = actions.agreements(second)
    secondAgreements should have size 1
    actions.agreements(third) should have size 0
    firstAgreements shouldBe secondAgreements
    val ag = firstAgreements.head.asInstanceOf[AllianceAgreement]
    ag.sides shouldBe Set(first, second)
    ag.signingTurn shouldBe actions.turn

    actions.relationships(first)(second) shouldBe (AllianceRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus)
    actions.relationships(second)(first) shouldBe (AllianceRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus)
  }

  test("simple decline alliance") {
    val List(first, second, third) = states

    val ap = new AllianceProposal(first, second)
    actions.sendMessage(ap)
    actions.mailbox(first) shouldBe Nil
    actions.mailbox(second) shouldBe List(ap)

    actions.answerMessage(ap, false)
    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val aa = firstMail.head.asInstanceOf[AllianceRejected]
    aa.from shouldBe second
    aa.to shouldBe first

    actions.agreements(first) should have size 0
    actions.agreements(second) should have size 0
    actions.agreements(third) should have size 0

    actions.relationships(first)(second) shouldBe (AllianceRejectionRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus)
    actions.relationships(second)(first) shouldBe (SamePriorityRelationshipBonus + SameReligionRelationshipBonus + + NeighboursWithoutClaimsRelationshipBonus)
  }
}
