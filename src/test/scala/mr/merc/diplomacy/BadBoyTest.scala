package mr.merc.diplomacy

import mr.merc.economics.WorldConstants

class BadBoyTest extends AbstractDiplomacyTest {

  test("Bad boy mechanic") {
    val List(first, second, _) = states
    val currentRelations12 = actions.relationships(first)(second)
    val currentRelations21 = actions.relationships(second)(first)

    actions.diplomacyEngine.increaseBadBoy(first, 10)
    val penalty = 10 * WorldConstants.Diplomacy.BadBoyToRelationsPenalty

    actions.relationships(first)(second) shouldBe currentRelations12
    actions.relationships(second)(first) shouldBe (currentRelations21 + penalty)

    actions.diplomacyEngine.improveBadBoyOverTime()

    val penalty2 = (10 - WorldConstants.Diplomacy.BadBoyTurnRecovery) * WorldConstants.Diplomacy.BadBoyToRelationsPenalty

    actions.relationships(first)(second) shouldBe currentRelations12
    actions.relationships(second)(first) shouldBe (currentRelations21 + penalty2)

  }
}
