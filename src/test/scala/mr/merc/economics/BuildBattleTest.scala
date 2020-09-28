package mr.merc.economics

import mr.merc.army.{Warrior, WarriorCompetence, WarriorType}
import mr.merc.economics.Culture.SlavicHuman
import mr.merc.map.terrain.FourSeasonsMapObjects.FourSeasonsHouse
import mr.merc.map.terrain.FourSeasonsTerrainTypes
import mr.merc.politics.{Party, Province, State}
import org.scalatest.{FunSuite, Matchers}

class BuildBattleTest extends FunSuite with Matchers {

  val state1:State = new State("1", Culture.LatinHuman, 0, new PoliticalSystem(Party.absolute))
  val state2:State = new State("2", Culture.FrenchHuman, 0, new PoliticalSystem(Party.absolute))
  val state3:State = new State("3", Culture.GreekHuman, 0, new PoliticalSystem(Party.absolute))
  val state4:State = new State("4", Culture.GermanHuman, 0, new PoliticalSystem(Party.absolute))

  var province1:Province = _
  var province2:Province = _
  var province3:Province = _
  var province4:Province = _

  test("build one-province battle") {
    val w1 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))
    val w2 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state2.primeCulture, state2))
    val w3 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state3.primeCulture, state3))
    val w4 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state4.primeCulture, state4))

    val hexField = build4ProvinceHexField()
    hexField.hex(0, 0).mapObj = Some(FourSeasonsHouse(SlavicHuman))

    val oneProvinceBattle = new OneProvinceBattle(hexField.buildTerrainHexField(Seasons.Summer), province1, Map(province3 -> w3), w1 ++ w2, Map(province4 -> w4), 0)
    val gameField = oneProvinceBattle.gameField

    gameField.players.toSet shouldBe Set(state1, state2, state3, state4).map(_.toPlayer)
    gameField.sides shouldBe Set(Set(state3.toPlayer), Set(state1, state2, state4).map(_.toPlayer))
    val province1Hexes = gameField.hexField.hexes.filter(_.province.contains(province1))
    province1Hexes.size shouldBe 9
    val province3Hexes = gameField.hexField.hexes.filter(_.province.contains(province3))
    province3Hexes.size shouldBe 3
    val province4Hexes = gameField.hexField.hexes.filter(_.province.contains(province4))
    province3Hexes.size shouldBe 3

    province3Hexes.map(h => (h.x, h.y)).toSet shouldBe Set((0, 3), (1, 3), (2, 3))
    province4Hexes.map(h => (h.x, h.y)).toSet shouldBe Set((3, 0), (3, 1), (3, 2))

    val expectedMilitia = gameField.hexField.hex(0, 0).soldier.get
    expectedMilitia.owner shouldBe state1.toPlayer
    expectedMilitia.soldierType.level shouldBe 1

    province1Hexes.flatMap(_.soldier).toSet shouldBe ((w1 ++ w2).map(_.soldier).toSet + expectedMilitia)
    province3Hexes.flatMap(_.soldier).toSet shouldBe w3.map(_.soldier).toSet
    province4Hexes.flatMap(_.soldier).toSet shouldBe w4.map(_.soldier).toSet
  }

  test("build two-province battle (one owner != controller)") {
    val hexField = build4ProvinceHexField()
    hexField.hex(4, 3).mapObj = Some(FourSeasonsHouse(SlavicHuman))
    province2.controller = state1
    hexField.hex(4, 1).mapObj = Some(FourSeasonsHouse(SlavicHuman))

    val w1 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state1.primeCulture, state1))
    val w2 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state2.primeCulture, state2))
    val w3 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state3.primeCulture, state3))
    val w4 = List(new Warrior(WarriorType.HeavyBladeInfantry, WarriorCompetence.Professional, state4.primeCulture, state4))

    val twoProvinceBattle = new TwoProvinceBattle(hexField.buildTerrainHexField(Seasons.Summer), province4, province2, Map(province2 -> w4), w2,
      Map(province3 -> w3), Map(province4 -> w1), Nil, Map(), 0)
    val gameField = twoProvinceBattle.gameField

    gameField.players.toSet shouldBe Set(state1, state2, state3, state4).map(_.toPlayer)
    gameField.sides shouldBe Set(Set(state4.toPlayer), Set(state1, state2, state3).map(_.toPlayer))

    val province2Hexes = gameField.hexField.hexes.filter(_.province.contains(province2))
    province2Hexes.size shouldBe 7
    val province3Hexes = gameField.hexField.hexes.filter(_.province.contains(province3))
    province3Hexes.size shouldBe 3
    val province4Hexes = gameField.hexField.hexes.filter(_.province.contains(province4))
    province4Hexes.size shouldBe 9

    val expectedMilitia = gameField.hexField.hex(2, 1).soldier.get
    expectedMilitia.owner shouldBe state4.toPlayer
    expectedMilitia.soldierType.level shouldBe 1

    gameField.hexField.hex(2, 3).soldier shouldBe None

    province2Hexes.map(h => (h.x, h.y)).toSet shouldBe Set((2,5), (3,4), (1,4), (1,3), (2,4), (3,3), (2,3))
    province3Hexes.map(h => (h.x, h.y)).toSet shouldBe Set((0, 3), (0, 4), (0, 5))
    province4Hexes.map(h => (h.x, h.y)).toSet shouldBe Set((3,1), (2,0), (3,0), (1,1), (3,2), (2,2), (1,2), (2,1), (1,0))

    province2Hexes.flatMap(_.soldier).toSet shouldBe w4.map(_.soldier).toSet
    province3Hexes.flatMap(_.soldier).toSet shouldBe w3.map(_.soldier).toSet
    province4Hexes.flatMap(_.soldier).toSet shouldBe (w1 ++ w2).map(_.soldier).toSet + expectedMilitia
  }

  def build4ProvinceHexField():FourSeasonsTerrainHexField = {
    val grid = new FourSeasonsTerrainHexField(6, 6, (x, y) => new FourSeasonsTerrainHex(x, y, FourSeasonsTerrainTypes.FourSeasonsGrass))

    def hexesFromTo(minX:Int, minY:Int, maxX:Int, maxY:Int):List[FourSeasonsTerrainHex] = {
      grid.hexes.filter(h => h.x >= minX && h.x < maxX && h.y >= minY && h.y < maxY).toList
    }

    province1 = new Province("1", state1, new RegionMarket(Map()), new RegionPopulation(Nil),
      hexesFromTo(0, 0, 3, 3).toSet, grid.hex(1, 1))
    province2 = new Province("2", state2, new RegionMarket(Map()), new RegionPopulation(Nil),
      hexesFromTo(3, 3, 6, 6).toSet, grid.hex(4, 4))
    province3 = new Province("3", state3, new RegionMarket(Map()), new RegionPopulation(Nil),
      hexesFromTo(0, 3, 3, 6).toSet, grid.hex(1, 4))
    province4 = new Province("4", state4, new RegionMarket(Map()), new RegionPopulation(Nil),
      hexesFromTo(3, 0, 6, 3).toSet, grid.hex(4, 1))

    def setProvinceToHexes(province: Province): Unit = {
      province.hexes.foreach(_.province = Some(province))
    }

    setProvinceToHexes(province1)
    setProvinceToHexes(province2)
    setProvinceToHexes(province3)
    setProvinceToHexes(province4)

    grid
  }
}
