package mr.merc.economics

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement
import mr.merc.diplomacy.DiplomaticAgreement.WarAgreement.TakeProvince
import mr.merc.economics.Culture.LatinHuman
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.terrain.GreenGrass
import mr.merc.politics.{Party, Province, State}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import mr.merc.map.terrain.FourSeasonsTerrainTypes._

class SoldierMovementAITest extends FunSuite with Matchers with BeforeAndAfter {

  val state1:State = new State("1", Culture.LatinHuman, 0, new PoliticalSystem(Party.absolute))
  val state2:State = new State("2", Culture.FrenchHuman, 0, new PoliticalSystem(Party.absolute))

  var province1:Province = _
  var province2:Province = _
  var province3:Province = _
  var province4:Province = _

  test("peace") {
    val warriors = List(
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))

    province1.regionWarriors.receiveWarriors(warriors)

    val worldState = new WorldState(List(province1, province2, province3, province4), state2,
      new FourSeasonsTerrainHexField(1, 1, (x,y) => new FourSeasonsTerrainHex(x, y, FourSeasonsGrass)), Map(), Stream())

    val soldierMovementAI = new SoldierMovementAI(worldState, state1)
    soldierMovementAI.moveSoldiers()

    val map = province1.regionWarriors.warriorDestinations
    val toProvince2 = map(Some(province2))
    val inProvince1 = map(None)
    (toProvince2 ++ inProvince1).toSet shouldBe warriors.toSet
    toProvince2.size should be > 0
  }

  test("war with to border movement") {
    val warriors = List(
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))

    province1.regionWarriors.receiveWarriors(warriors)

    val worldState = new WorldState(List(province1, province2, province3, province4), state2,
      new FourSeasonsTerrainHexField(1, 1, (x,y) => new FourSeasonsTerrainHex(x, y, FourSeasonsGrass)), Map(), Stream())

    worldState.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province3)), ""))

    val soldierMovementAI = new SoldierMovementAI(worldState, state1)
    soldierMovementAI.moveSoldiers()

    val map = province1.regionWarriors.warriorDestinations
    val toProvince2 = map(Some(province2))
    val inProvince1 = map(None)
    inProvince1 shouldBe Nil
    toProvince2.toSet shouldBe warriors.toSet
  }

  test("war with attack") {
    val w1 = List(
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))

    province2.regionWarriors.receiveWarriors(w1)

    val w2 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state2.primeCulture, state2))

    province3.regionWarriors.receiveWarriors(w2)

    val worldState = new WorldState(List(province1, province2, province3, province4), state2,
      new FourSeasonsTerrainHexField(1, 1, (x,y) => new FourSeasonsTerrainHex(x, y, FourSeasonsGrass)), Map(), Stream())

    worldState.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province3)), ""))

    val soldierMovementAI = new SoldierMovementAI(worldState, state1)
    soldierMovementAI.moveSoldiers()

    val map = province2.regionWarriors.warriorDestinations
    val attackers = map(Some(province3))
    attackers shouldBe w1
  }

  test("war without attack") {
    val w1 = List(
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))

    province2.regionWarriors.receiveWarriors(w1)

    val w2 = List(
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state2.primeCulture, state2),
      new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state2.primeCulture, state2))

    province3.regionWarriors.receiveWarriors(w2)

    province3.regionPopulation.pop(Population.Aristocrats, LatinHuman).applyMovers(PopulationMovers(WorldConstants.Population.HousePerPopulation * 4, 0))

    val worldState = new WorldState(List(province1, province2, province3, province4), state2,
      new FourSeasonsTerrainHexField(1, 1, (x,y) => new FourSeasonsTerrainHex(x, y, FourSeasonsGrass)), Map(), Stream())

    worldState.diplomacyEngine.addAgreement(new WarAgreement(Set(state1), Set(state2), state1, state2, 0,
      Set(new TakeProvince(state1, state2, province3)), ""))

    val soldierMovementAI = new SoldierMovementAI(worldState, state1)
    soldierMovementAI.moveSoldiers()

    val map = province2.regionWarriors.warriorDestinations
    val inProvince2 = map(None)
    inProvince2 shouldBe w1
  }

  before {
    init4ProvinceHexField()
  }

  def init4ProvinceHexField():Unit = {
    province1 = new Province("1", state1, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province2 = new Province("2", state1, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province3 = new Province("3", state2, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))
    province4 = new Province("4", state2, new RegionMarket(Map()), new RegionPopulation(Nil),
      Set(), new FourSeasonsTerrainHex(0, 0, FourSeasonsGrass))

    province1.initNeighbours(Set(province2))
    province2.initNeighbours(Set(province1, province3))
    province3.initNeighbours(Set(province2, province4))
    province4.initNeighbours(Set(province3))
  }
}
