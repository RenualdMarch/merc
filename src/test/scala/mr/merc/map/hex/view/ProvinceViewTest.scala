package mr.merc.map.hex.view

import mr.merc.army.WarriorCompetence.{Militia, Professional}
import mr.merc.army.{Warrior, WarriorViewNames}
import mr.merc.army.WarriorType.{HeavyBladeInfantry, HeavyMaceInfantry}
import mr.merc.economics.Culture._
import mr.merc.economics.Population.Humans
import mr.merc.economics.Seasons.Summer
import mr.merc.economics._
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.terrain.{Castle, FourSeasonsTerrainTypes, ShallowWater}
import mr.merc.map.view.SoldiersDrawer
import mr.merc.politics.{Party, Province, State}
import mr.merc.unit.Soldier
import mr.merc.unit.view.SoldierView
import org.scalatest.{BeforeAndAfter, FunSuite, Inspectors, Matchers}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar
import mr.merc.economics.WorldConstants.Population._
import mr.merc.map.objects.House
import mr.merc.map.terrain.FourSeasonsMapObjects.FourSeasonsHouse
import scalafx.scene.paint.Color

class ProvinceViewTest extends FunSuite with MockitoSugar with BeforeAndAfter with Inspectors with Matchers {

  val field = new FourSeasonsTerrainHexField(5, 5, (x, y) => new FourSeasonsTerrainHex(x, y, FourSeasonsTerrainTypes.FourSeasonsGrass))
  field.hex(3, 1).terrainMap = FourSeasonsTerrainTypes.FourSeasonsRiver
  field.hex(3, 2).terrainMap = FourSeasonsTerrainTypes.FourSeasonsRiver
  field.hex(2, 2).terrainMap = FourSeasonsTerrainTypes.FourSeasonsCastle

  val province = mock[Province]
  val population = mock[RegionPopulation]

  val testCulture = new Culture("testCulture", Humans, "testHouse", Color.White) {
    override val warriorViewNames: WarriorViewNames = WarriorViewNames(Map(
      (HeavyMaceInfantry, Professional) -> "oneImageSoldier",
      (HeavyMaceInfantry, Militia) -> "testSoldier2"))
    override val cultureInfo: CultureInfo = null
  }

  val state = new State("", testCulture, 0, new PoliticalSystem(Party.absolute))

  // this test assumes that HumanCityHouse is used for LatinHuman culture and HumanVillageHouse for western humans

  test("fill and refill") {
    when(province.regionPopulation).thenReturn(population)
    when(province.capital).thenReturn(field.hex(2, 2))
    when(province.hexes).thenReturn(field.hexes.toSet)
    val firstMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3)
    val secondMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3, FrenchHuman -> 1)
    val thirdMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 2, FrenchHuman -> HousePerPopulation)
    val fourthMap = Map[Culture, Int](LatinHuman -> HousePerPopulation * 3, FrenchHuman -> HousePerPopulation)


    when(population.cultureMembers).thenReturn(firstMap, secondMap, thirdMap, fourthMap)
    val provinceView = new ProvinceView(Summer, province, field, field.buildTerrainHexField(Summer), new TerrainHexFieldView(field.buildTerrainHexField(Seasons.Summer), new SoldiersDrawer[SoldierView], 1))

    provinceView.refreshCity()
    val houses1 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))
    assert(houses1.size === 3)
    assert(houses1.forall(_.mapObj.contains(FourSeasonsHouse(LatinHuman))))
    assert(field.hex(3, 1).mapObj.isEmpty)
    assert(field.hex(3, 2).mapObj.isEmpty)
    provinceView.refreshCity()

    val houses2 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))
    assert(houses2.size === 4)
    val latinHouses2 = houses2.filter(_.mapObj.contains(FourSeasonsHouse(LatinHuman)))
    val westHouses2 = houses2.filter(_.mapObj.contains(FourSeasonsHouse(FrenchHuman)))
    assert(latinHouses2.size === 3)
    assert(westHouses2.size === 1)

    assert(latinHouses2 === houses1)

    provinceView.refreshCity()
    val houses3 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))
    assert(houses3.size === 3)

    val latinHouses3 = houses3.filter(_.mapObj.contains(FourSeasonsHouse(LatinHuman)))
    val westHouses3 = houses3.filter(_.mapObj.contains(FourSeasonsHouse(FrenchHuman)))

    assert(latinHouses3.size === 2)
    assert(westHouses3.size === 1)

    assert(latinHouses3.toSet.subsetOf(latinHouses2.toSet))
    assert(westHouses2.toSet.subsetOf(westHouses3.toSet))

    provinceView.refreshCity()
    val houses4 = field.hexes.filter(_.mapObj.exists(_.isInstanceOf[FourSeasonsHouse]))
    assert(houses4.size === 4)
    val latinHouses4 = houses4.filter(_.mapObj.contains(FourSeasonsHouse(LatinHuman)))
    val westHouses4 = houses4.filter(_.mapObj.contains(FourSeasonsHouse(FrenchHuman)))

    assert(latinHouses4.size === 3)
    assert(westHouses4.size === 1)

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
    val provinceView = new ProvinceView(Summer, province, field, field.buildTerrainHexField(Summer), new TerrainHexFieldView(field.buildTerrainHexField(Summer), sd, 1))

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
