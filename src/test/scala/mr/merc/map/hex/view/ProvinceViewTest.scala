package mr.merc.map.hex.view

import mr.merc.army.{Warrior, WarriorViewNames}
import mr.merc.army.WarriorType.{HeavyBladeInfantry, HeavyMaceInfantry, Militia, Professional}
import mr.merc.economics.Population.{Culture, Humans, KnightHuman, LatinHuman}
import mr.merc.economics.{EconomicRegion, PoliticalSystem, RegionPopulation, RegionWarriors}
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.{House, HumanCityHouse, HumanVillageHouse}
import mr.merc.map.terrain.{Castle, ShallowWater}
import mr.merc.map.view.SoldiersDrawer
import mr.merc.politics.{Party, Province, State}
import mr.merc.unit.Soldier
import mr.merc.unit.view.SoldierView
import org.scalatest.{BeforeAndAfter, FunSuite, Inspectors, Matchers}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import mr.merc.economics.WorldConstants.Population._
import scalafx.scene.paint.Color

class ProvinceViewTest extends FunSuite with MockitoSugar with BeforeAndAfter with Inspectors with Matchers {

  val field = new TerrainHexField(5, 5, TerrainHex.grassInit)
  field.hex(3, 1).terrain = ShallowWater
  field.hex(3, 2).terrain = ShallowWater
  field.hex(2, 2).terrain = Castle

  val province = mock[Province]
  val population = mock[RegionPopulation]

  val testCulture = new Culture("testCulture", Humans, HumanCityHouse, Color.White, WarriorViewNames(Map(
    (HeavyMaceInfantry, Professional) -> "oneImageSoldier",
    (HeavyMaceInfantry, Militia) -> "testSoldier2"))) {}

  val state = new State("", testCulture, 0, new PoliticalSystem(Party.absolute))

  // this test assumes that HumanCityHouse is used for LatinHuman culture and HumanVillageHouse for western humans

  test("fill and refill") {
    when(province.regionPopulation).thenReturn(population)
    when(province.capital).thenReturn(field.hex(2, 2))
    when(province.hexes).thenReturn(field.hexes.toSet)
    val firstMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3)
    val secondMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3, KnightHuman -> 1)
    val thirdMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 2, KnightHuman -> HousePerPopulation)
    val fourthMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3, KnightHuman -> HousePerPopulation)


    when(population.cultureMembers).thenReturn(firstMap, secondMap, thirdMap, fourthMap)
    val provinceView = new ProvinceView(province, field, new TerrainHexFieldView(field, new SoldiersDrawer[SoldierView], 1))

    provinceView.refreshCity()
    val houses1 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    assert(houses1.size === 4)
    assert(houses1.forall(_.mapObj.contains(HumanCityHouse)))
    assert(field.hex(3, 1).mapObj.isEmpty)
    assert(field.hex(3, 2).mapObj.isEmpty)
    provinceView.refreshCity()

    val houses2 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    assert(houses2.size === 5)
    val latinHouses2 = houses2.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses2 = houses2.filter(_.mapObj.contains(HumanVillageHouse))
    assert(latinHouses2.size === 4)
    assert(westHouses2.size === 1)

    assert(latinHouses2 === houses1)

    provinceView.refreshCity()
    val houses3 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    assert(houses3.size === 5)

    val latinHouses3 = houses3.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses3 = houses3.filter(_.mapObj.contains(HumanVillageHouse))

    assert(latinHouses3.size === 3)
    assert(westHouses3.size === 2)

    assert(latinHouses3.toSet.subsetOf(latinHouses2.toSet))
    assert(westHouses2.toSet.subsetOf(westHouses3.toSet))

    provinceView.refreshCity()
    val houses4 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[House]))
    assert(houses4.size === 6)
    val latinHouses4 = houses4.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses4 = houses4.filter(_.mapObj.contains(HumanVillageHouse))

    assert(latinHouses4.size === 4)
    assert(westHouses4.size === 2)

    assert(latinHouses3.toSet.subsetOf(latinHouses4.toSet))
    assert(westHouses3.toSet.subsetOf(westHouses4.toSet))
  }

  test("soldiers") {
    val region = mock[Province]
    val state = new State("", LatinHuman, 0, new PoliticalSystem(Party.absolute))
    val regionWarriors = new RegionWarriors(Nil, Set(region))
    when(province.regionWarriors).thenReturn(regionWarriors)
    when(province.regionPopulation).thenReturn(population)
    when(province.capital).thenReturn(field.hex(2, 2))
    when(province.hexes).thenReturn(field.hexes.toSet)
    when(province.owner).thenReturn(state)
    val firstMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3)
    when(population.cultureMembers).thenReturn(firstMap)

    val sd = new SoldiersDrawer[SoldierView]()
    val provinceView = new ProvinceView(province, field, new TerrainHexFieldView(field, sd, 1))

    forAll(field.hexes) { h =>
      h.soldier shouldBe None
    }

    val warrior = new Warrior(HeavyMaceInfantry, Professional, testCulture, state)
    province.regionWarriors.receiveWarriors(List(warrior))

    provinceView.refreshSoldiers()

    forExactly(1, field.hexes) { h =>
      h.soldier shouldBe Some(warrior.soldier)
    }

    val wasSoldier = field.hexes.find(_.soldier.nonEmpty).get

    sd.soldiers shouldBe Set(warrior.soldierView(1d, false))

    field.hex(4, 4).province = Some(region)
    regionWarriors.planSendWarriors(List(warrior), Some(region))
    provinceView.refreshSoldiers()

    forExactly(1, field.hexes) { h =>
      h.soldier shouldBe Some(warrior.soldier)
    }

    val isSoldier = field.hexes.find(_.soldier.nonEmpty).get
    wasSoldier should not be isSoldier

    sd.soldiers shouldBe Set(warrior.soldierView(1d, false))

    regionWarriors.sendWarriorsToDestinations()
    provinceView.refreshSoldiers()

    forAll(field.hexes) { h =>
      h.soldier shouldBe None
    }
    sd.soldiers shouldBe Set()

  }
}
