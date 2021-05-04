package mr.merc.economics

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.TakeProvince
import mr.merc.map.terrain.FourSeasonsTerrainTypes
import mr.merc.players.{ColorGenerator, NamesGenerator}
import mr.merc.politics.{Party, Province, State}
import org.scalatest.{FunSuite, Matchers}
import FourSeasonsTerrainTypes._

class MovementAndBattlesResolverTest extends FunSuite with Matchers {

  val state1:State = new State("1", Culture.LatinHuman, 0, Party.absolute, 0)
  val state2:State = new State("2", Culture.LatinHuman, 0, Party.absolute, 0)
  val state3:State = new State("3", Culture.LatinHuman, 0, Party.absolute, 0)
  val state4:State = new State("4", Culture.LatinHuman, 0, Party.absolute, 0)

  var province1:Province = _
  var province2:Province = _
  var province3:Province = _
  var province4:Province = _

  test("simple movement") {
    val ws = build4ProvinceWorldState()

    province2.owner = state1
    province2.controller = state1

    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))

    val w = province1.regionWarriors.allWarriors
    province1.regionWarriors.planSendWarriors(w, Some(province2))

    val resolver = new MovementAndBattlesResolver(ws)
    resolver.moveAndPrepareBattles() shouldBe Nil

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe w
  }

  test("simple one province battle") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province2)), ""))

    val w = province1.regionWarriors.allWarriors
    province1.regionWarriors.planSendWarriors(w, Some(province2))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil

    battle.provinces shouldBe List(province2)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers shouldBe Map(province1 -> w)
    oneProvinceBattle.defenders shouldBe Nil
    oneProvinceBattle.additionalDefenders shouldBe Map()
  }

  test("complex one province battle") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1, state2), Set(state3, state4), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province2.regionWarriors.planSendWarriors(w2, Some(province3))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil
    province4.regionWarriors.allWarriors shouldBe Nil

    battle.provinces shouldBe List(province3)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers shouldBe Map(province1 -> w1, province2 -> w2)
    oneProvinceBattle.defenders shouldBe w3
    oneProvinceBattle.additionalDefenders shouldBe Map(province4 -> w4)
  }

  test("only one possible one province battle") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state3, state4), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))
    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state2), Set(state3, state4), state2, state3, 0,
      Set(new TakeProvince(state2, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province2.regionWarriors.planSendWarriors(w2, Some(province3))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    battle.provinces shouldBe List(province3)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers shouldBe Map(province2 -> w2)
    oneProvinceBattle.defenders shouldBe w3
    oneProvinceBattle.additionalDefenders shouldBe Map(province4 -> w4)

    province1.regionWarriors.allWarriors shouldBe w1
    province2.regionWarriors.allWarriors shouldBe Nil
    province4.regionWarriors.allWarriors shouldBe Nil
  }

  test("one province battle with allies from one province") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1, state2), Set(state3, state4), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil
    province4.regionWarriors.allWarriors shouldBe Nil

    battle.provinces shouldBe List(province3)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers.mapValues(_.toSet) shouldBe Map(province1 -> w1.toSet)
    oneProvinceBattle.defenders shouldBe w3
    oneProvinceBattle.additionalDefenders shouldBe Map(province4 -> w4)
  }

  test("one province battle without allies") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state3, state4), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))
    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state2), Set(state3, state4), state2, state3, 0,
      Set(new TakeProvince(state2, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w11 = w1.filter(_.owner == state1)
    val w12 = w1.filter(_.owner == state2)
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    battle.provinces shouldBe List(province3)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers shouldBe Map(province1 -> w12)
    oneProvinceBattle.defenders shouldBe w3
    oneProvinceBattle.additionalDefenders shouldBe Map(province4 -> w4)

    province1.regionWarriors.allWarriors shouldBe w11
    province2.regionWarriors.allWarriors shouldBe Nil
    province4.regionWarriors.allWarriors shouldBe Nil
  }

  test("complex one province battle without allies") {
    val ws = build4ProvinceWorldState()
    province2.owner = state1
    province2.controller = state1

    province4.owner = state3
    province4.owner = state3

    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state3), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province2.regionWarriors.planSendWarriors(w2, Some(province3))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil
    province4.regionWarriors.allWarriors shouldBe Nil

    battle.provinces shouldBe List(province3)
    val oneProvinceBattle = battle.asInstanceOf[OneProvinceBattle]
    oneProvinceBattle.attackers shouldBe Map(province1 -> w1, province2 -> w2)
    oneProvinceBattle.defenders shouldBe w3
    oneProvinceBattle.additionalDefenders shouldBe Map(province4 -> w4)
  }

  test("simple two province battle") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province2)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province2))
    province2.regionWarriors.planSendWarriors(w2, Some(province1))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil

    battle.provinces.toSet shouldBe Set(province1, province2)
    val twoProvinceBattle = battle.asInstanceOf[TwoProvinceBattle]
    if(twoProvinceBattle.province1 == province1 && twoProvinceBattle.province2 == province2) {
      twoProvinceBattle.province1Attackers shouldBe Map(province2 -> w2)
      twoProvinceBattle.province2Attackers shouldBe Map(province1 -> w1)
      twoProvinceBattle.province1AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province2AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province1Defenders shouldBe Nil
      twoProvinceBattle.province2Defenders shouldBe Nil
    } else if (twoProvinceBattle.province2 == province1 && twoProvinceBattle.province1 == province2) {
      twoProvinceBattle.province1Attackers shouldBe Map(province1 -> w2)
      twoProvinceBattle.province2Attackers shouldBe Map(province2 -> w1)
      twoProvinceBattle.province1AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province2AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province1Defenders shouldBe Nil
      twoProvinceBattle.province2Defenders shouldBe Nil
    } else fail("impossible case")
  }

  test("complex two province battle") {
    val ws = build4ProvinceWorldState()
    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1, state2), Set(state3, state4), state1, state3, 0,
      Set(new TakeProvince(state1, state3, province3)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3Init = province3.regionWarriors.allWarriors.init
    val w3Last = province3.regionWarriors.allWarriors.last :: Nil
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province3))
    province2.regionWarriors.planSendWarriors(w2, Some(province3))
    province3.regionWarriors.planSendWarriors(w3Init, Some(province1))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val List(battle) = resolver.moveAndPrepareBattles()

    province1.regionWarriors.allWarriors shouldBe Nil
    province2.regionWarriors.allWarriors shouldBe Nil
    province3.regionWarriors.allWarriors shouldBe w3Last
    province4.regionWarriors.allWarriors shouldBe Nil

    battle.provinces.toSet shouldBe Set(province1, province3)
    val twoProvinceBattle = battle.asInstanceOf[TwoProvinceBattle]

    if(twoProvinceBattle.province1 == province1 && twoProvinceBattle.province2 == province3) {
      twoProvinceBattle.province1Attackers shouldBe Map(province3 -> w3Init)
      twoProvinceBattle.province2Attackers shouldBe Map(province1 -> w1, province2 -> w2)
      twoProvinceBattle.province1AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province2AdditionalDefenders shouldBe Map(province4 -> w4)
      twoProvinceBattle.province1Defenders shouldBe Nil
      twoProvinceBattle.province2Defenders shouldBe w3Last
    } else if (twoProvinceBattle.province2 == province3 && twoProvinceBattle.province2 == province1) {
      twoProvinceBattle.province1Attackers shouldBe Map(province1 -> w1, province2 -> w2)
      twoProvinceBattle.province2Attackers shouldBe Map(province3 -> w3Init)
      twoProvinceBattle.province1AdditionalDefenders shouldBe Map(province4 -> w4)
      twoProvinceBattle.province2AdditionalDefenders shouldBe Map()
      twoProvinceBattle.province1Defenders shouldBe w3Last
      twoProvinceBattle.province2Defenders shouldBe Nil
    } else fail("impossible case")
  }

  test("two one province battles") {
    val ws = build4ProvinceWorldState()

    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province2)), ""))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state3), Set(state4), state3, state4, 0,
      Set(new TakeProvince(state3, state4, province4)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province2))
    province3.regionWarriors.planSendWarriors(w3, Some(province4))

    val resolver = new MovementAndBattlesResolver(ws)
    val battles = resolver.moveAndPrepareBattles()
    battles.size shouldBe 2

    val b1 = battles.find(_.provinces == List(province2)).get.asInstanceOf[OneProvinceBattle]
    val b2 = battles.find(_.provinces == List(province4)).get.asInstanceOf[OneProvinceBattle]

    b1.attackers shouldBe Map(province1 -> w1)
    b1.defenders shouldBe w2
    b1.additionalDefenders shouldBe Map()

    b2.attackers shouldBe Map(province3 -> w3)
    b2.defenders shouldBe w4
    b2.additionalDefenders shouldBe Map()

  }

  test("two two province battles") {
    val ws = build4ProvinceWorldState()

    province1.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state1.primeCulture, state1)))
    province2.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2), new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state2.primeCulture, state2)))
    province3.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state3.primeCulture, state3)))
    province4.regionWarriors.receiveWarriors(List(new Warrior(WarriorType.HeavyBladeInfantry,
      WarriorCompetence.Professional, state4.primeCulture, state4)))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province2)), ""))

    ws.diplomacyEngine.addAgreement(new WarAgreement(Set(state3), Set(state4), state3, state4, 0,
      Set(new TakeProvince(state3, state4, province4)), ""))

    val w1 = province1.regionWarriors.allWarriors
    val w2 = province2.regionWarriors.allWarriors
    val w3 = province3.regionWarriors.allWarriors
    val w4 = province4.regionWarriors.allWarriors

    province1.regionWarriors.planSendWarriors(w1, Some(province2))
    province2.regionWarriors.planSendWarriors(w2, Some(province1))
    province3.regionWarriors.planSendWarriors(w3, Some(province4))
    province4.regionWarriors.planSendWarriors(w4, Some(province3))

    val resolver = new MovementAndBattlesResolver(ws)
    val battles = resolver.moveAndPrepareBattles()
    battles.size shouldBe 2

    val b1 = battles.find(_.provinces.toSet == Set(province1, province2)).get.asInstanceOf[TwoProvinceBattle]
    val b2 = battles.find(_.provinces.toSet == Set(province3, province4)).get.asInstanceOf[TwoProvinceBattle]

    if (b1.province1 == province1) {
      b1.province1Attackers shouldBe Map(province2 -> w2)
      b1.province2Attackers shouldBe Map(province1 -> w1)
    } else if (b1.province1 == province2) {
      b1.province1Attackers shouldBe Map(province1 -> w1)
      b1.province2Attackers shouldBe Map(province2 -> w2)
    } else fail("Impossible case")

    if (b2.province1 == province3) {
      b2.province1Attackers shouldBe Map(province4 -> w4)
      b2.province2Attackers shouldBe Map(province3 -> w3)
    } else if (b2.province1 == province4) {
      b2.province1Attackers shouldBe Map(province3 -> w3)
      b2.province2Attackers shouldBe Map(province4 -> w4)
    } else fail("Impossible case")
  }

  def build4ProvinceWorldState():WorldState = {
    province1 = new Province("1", state1, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass)), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province2 = new Province("2", state2, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass)), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province3 = new Province("3", state3, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass)), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province4 = new Province("4", state4, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass)), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))

    province1.initNeighbours(Set(province2, province3, province4))
    province2.initNeighbours(Set(province1, province3, province4))
    province3.initNeighbours(Set(province1, province2, province4))
    province4.initNeighbours(Set(province1, province2, province3))

    val colorStream = ColorGenerator.colorStream
    val namesGenerators:Map[Culture, NamesGenerator] = Culture.cultures.map { c =>
      c -> new NamesGenerator(c.cultureInfo)
    } toMap

    val worldState = new WorldState(List(province1, province2, province3, province4), state1, new FourSeasonsTerrainHexField(1, 1,
      (_, _) => new FourSeasonsTerrainHex(0, 0, FourSeasonsTerrainTypes.FourSeasonsGrass)), namesGenerators, colorStream)
    worldState
  }
}
