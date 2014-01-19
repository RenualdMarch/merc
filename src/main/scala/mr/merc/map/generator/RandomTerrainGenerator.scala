package mr.merc.map.generator

import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._
import scala.util.Random

class RandomTerrainGenerator extends MapGenerator {

  val terrainTypesMap = Map(Water -> 1, Forest -> 1, Grass -> 3, Road -> 1, Sand -> 1, Swamp -> 1, Hill -> 1, Mountain -> 1)
  val terrainTypes: List[TerrainType] = terrainTypesMap.flatMap { case (t, i) => List.fill(i)(t) } toList
  val houseChance = 0.1
  val bridgeChance = 0.1

  def generateMap(width: Int, height: Int, seed: Int): TerrainHexField = {
    val random = new Random(seed)
    def terrainHexFunction(x: Int, y: Int): TerrainHex = {
      val terrain = terrainTypes(random.nextInt(terrainTypes.size))
      val mapObj = if (terrain == Grass) {
        if (random.nextDouble < houseChance) {
          Some(House)
        } else {
          None
        }
      } else if (terrain == Water || terrain == Swamp) {
        if (random.nextDouble < bridgeChance) {
          Some(WoodenBridge)
        } else {
          None
        }
      } else {
        None
      }
      new TerrainHex(x, y, terrain, mapObj)
    }

    new TerrainHexField(width, height, terrainHexFunction)
  }

}