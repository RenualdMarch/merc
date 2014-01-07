package mr.merc.map.generator

import mr.merc.map.hex.TerrainHexField
import scala.util.Random

trait MapGenerator {
  def generateMap(width: Int, height: Int, seed: Int = Random.nextInt): TerrainHexField
}