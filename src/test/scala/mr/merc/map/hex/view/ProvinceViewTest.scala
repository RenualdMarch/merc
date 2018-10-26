package mr.merc.map.hex.view

import mr.merc.economics.Population.{Culture, LatinHuman, WesternHuman}
import mr.merc.economics.RegionPopulation
import mr.merc.map.hex.{TerrainHex, TerrainHexField}
import mr.merc.map.objects.{HumanCityHouse, HumanVillageHouse}
import mr.merc.map.terrain.{Castle, ShallowWater}
import mr.merc.map.view.SoldiersDrawer
import mr.merc.politics.Province
import mr.merc.unit.Soldier
import mr.merc.unit.view.SoldierView
import org.scalatest.{BeforeAndAfter, FunSuite}
import org.mockito.Mockito._
import org.scalatest.mockito.MockitoSugar

class ProvinceViewTest extends FunSuite with MockitoSugar with BeforeAndAfter {

  val field = new TerrainHexField(5, 5, TerrainHex.grassInit)
  field.hex(3, 1).terrain = ShallowWater
  field.hex(3, 2).terrain = ShallowWater
  field.hex(2, 2).terrain = Castle

  val province = mock[Province]
  val population = mock[RegionPopulation]


  // this test assumes that HumanCityHouse is used for LatinHuman culture and HumanVillageHouse for western humans

  test("fill and refill") {
    when(province.regionPopulation).thenReturn(population)
    when(province.capital).thenReturn(field.hex(2, 2))
    when(province.hexes).thenReturn(field.hexes.toSet)
    val firstMap = Map[Culture, Int](LatinHuman -> ProvinceView.housePerPopulation * 3)
    val secondMap = Map[Culture, Int](LatinHuman -> ProvinceView.housePerPopulation * 3, WesternHuman -> 1)
    val thirdMap = Map[Culture, Int](LatinHuman -> ProvinceView.housePerPopulation * 2, WesternHuman -> ProvinceView.housePerPopulation)
    val fourthMap = Map[Culture, Int](LatinHuman -> ProvinceView.housePerPopulation * 3, WesternHuman -> ProvinceView.housePerPopulation)


    when(population.cultureMembers).thenReturn(firstMap, secondMap, thirdMap, fourthMap)
    val provinceView = new ProvinceView(province, field, new TerrainHexFieldView(field, new SoldiersDrawer[SoldierView], 1))

    provinceView.refreshCity()
    val houses1 = field.hexes.filter(_.mapObj.nonEmpty)
    assert(houses1.size === 4)
    assert(houses1.forall(_.mapObj.contains(HumanCityHouse)))
    assert(field.hex(3, 1).mapObj.isEmpty)
    assert(field.hex(3, 2).mapObj.isEmpty)
    provinceView.refreshCity()

    val houses2 = field.hexes.filter(_.mapObj.nonEmpty)
    assert(houses2.size === 5)
    val latinHouses2 = houses2.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses2 = houses2.filter(_.mapObj.contains(HumanVillageHouse))
    assert(latinHouses2.size === 4)
    assert(westHouses2.size === 1)

    assert(latinHouses2 === houses1)

    provinceView.refreshCity()
    val houses3 = field.hexes.filter(_.mapObj.nonEmpty)
    assert(houses3.size === 5)

    val latinHouses3 = houses3.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses3 = houses3.filter(_.mapObj.contains(HumanVillageHouse))

    assert(latinHouses3.size === 3)
    assert(westHouses3.size === 2)

    assert(latinHouses3.toSet.subsetOf(latinHouses2.toSet))
    assert(westHouses2.toSet.subsetOf(westHouses3.toSet))

    provinceView.refreshCity()
    val houses4 = field.hexes.filter(_.mapObj.nonEmpty)
    assert(houses4.size === 6)
    val latinHouses4 = houses4.filter(_.mapObj.contains(HumanCityHouse))
    val westHouses4 = houses4.filter(_.mapObj.contains(HumanVillageHouse))

    assert(latinHouses4.size === 4)
    assert(westHouses4.size === 2)

    assert(latinHouses3.toSet.subsetOf(latinHouses4.toSet))
    assert(westHouses3.toSet.subsetOf(westHouses4.toSet))
  }
}
