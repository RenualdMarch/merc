package mr.merc.diplomacy

import mr.merc.diplomacy.DiplomaticAgreement.VassalAgreement
import mr.merc.diplomacy.DiplomaticMessage._
import mr.merc.economics.WorldConstants.Diplomacy._

class VassalTest extends AbstractDiplomacyTest {

  test("accept vassal offer") {
    val List(first, second, third) = states
    val vp = new VassalizationProposal(first, second)

    sendAndAccept(vp)

    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val va = firstMail.head.asInstanceOf[VassalizationAccepted]
    va.from shouldBe second
    va.to shouldBe first

    val firstAgreements = actions.agreements(first)
    firstAgreements should have size 1
    val secondAgreements = actions.agreements(second)
    secondAgreements should have size 1
    actions.agreements(third) should have size 0
    firstAgreements shouldBe secondAgreements
    val vag = firstAgreements.head.asInstanceOf[VassalAgreement]
    vag.sides shouldBe Set(first, second)
    vag.signingTurn shouldBe actions.turn
    vag.vassal shouldBe second
    vag.overlord shouldBe first

    actions.relationships(first)(second) shouldBe OverlordRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus
    actions.relationships(second)(first) shouldBe VassalRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus
  }

  test("decline vassal offer") {
    val List(first, second, third) = states

    val vp = new VassalizationProposal(first, second)

    sendAndDecline(vp)

    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val va = firstMail.head.asInstanceOf[VassalizationRejected]
    va.from shouldBe second
    va.to shouldBe first

    actions.agreements(first) should have size 0
    actions.agreements(second) should have size 0
    actions.agreements(third) should have size 0

    actions.relationships(first)(second) shouldBe VassalRejectionRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus+ NeighboursWithoutClaimsRelationshipBonus
    actions.relationships(second)(first) shouldBe SamePriorityRelationshipBonus + SameReligionRelationshipBonus+ NeighboursWithoutClaimsRelationshipBonus
  }

  test("propose overlordship") {
    val List(first, second, third) = states
    val vp = new OverlordshipProposal(first, second)

    sendAndAccept(vp)

    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val va = firstMail.head.asInstanceOf[OverlordshipAccepted]
    va.from shouldBe second
    va.to shouldBe first

    val firstAgreements = actions.agreements(first)
    firstAgreements should have size 1
    val secondAgreements = actions.agreements(second)
    secondAgreements should have size 1
    actions.agreements(third) should have size 0
    firstAgreements shouldBe secondAgreements
    val vag = firstAgreements.head.asInstanceOf[VassalAgreement]
    vag.sides shouldBe Set(first, second)
    vag.signingTurn shouldBe actions.turn

    actions.relationships(first)(second) shouldBe VassalRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus
    actions.relationships(second)(first) shouldBe OverlordRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus

    vag.vassal shouldBe first
    vag.overlord shouldBe second
  }

  test("decline overlordship") {
    val List(first, second, third) = states

    val vp = new OverlordshipProposal(first, second)

    sendAndDecline(vp)

    val firstMail = actions.mailbox(first)
    actions.mailbox(second) shouldBe Nil
    firstMail should have size 1
    val va = firstMail.head.asInstanceOf[OverlordshipRejected]
    va.from shouldBe second
    va.to shouldBe first

    actions.agreements(first) should have size 0
    actions.agreements(second) should have size 0
    actions.agreements(third) should have size 0

    actions.relationships(first)(second) shouldBe VassalRejectionRelationshipChange + SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus
    actions.relationships(second)(first) shouldBe SamePriorityRelationshipBonus + SameReligionRelationshipBonus + NeighboursWithoutClaimsRelationshipBonus
  }
}
