package mr.merc.map.generator

import mr.merc.economics.Culture.LatinHuman
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain._
import mr.merc.map.objects._

import scala.util.Random

class RandomTerrainGenerator(houseChance: Double = 0.1, bridgeChance: Double = 0.1) extends MapGenerator {

  val terrainTypesMap = Map[TerrainType, Int](ShallowWater -> 1, DecForest -> 1, GreenGrass -> 3, CleanRoad -> 1,
    DesertSand -> 1, Mud -> 1, BasicHill -> 1, BasicMountain -> 1, DirtRoad -> 1, Castle -> 1)
  val terrainTypes: List[TerrainType] = terrainTypesMap.flatMap { case (t, i) => List.fill(i)(t) } toList

  def generateMap(width: Int, height: Int, seed: Int): TerrainHexField = {
    val random = new Random(seed)
    def terrainHexFunction(x: Int, y: Int): TerrainHex = {
      val terrain = terrainTypes(random.nextInt(terrainTypes.size))
      val mapObj = if (terrain.is(GrassKind)) {
        if (random.nextDouble < houseChance) {
          Some(SummerHouse(LatinHuman))
        } else {
          None
        }
      } else if (terrain.is(WaterKind) || terrain.is(SwampKind)) {
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