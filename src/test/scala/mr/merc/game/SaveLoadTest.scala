package mr.merc.game

import java.io.File

import mr.merc.economics.WorldGenerationConstants.WorldMapCreationConf
import mr.merc.economics.WorldGenerator
import org.scalatest.{FunSuite, Matchers}

class SaveLoadTest extends FunSuite with Matchers {

  test("should save and load") {
    val ws = WorldGenerator.generateWorld(WorldMapCreationConf(30, 30, 100))
    val gc = GameContainer(ws)
    val file = File.createTempFile("aaa", "bbb")
    SaveLoad.save(gc, file)
    val gc2 = SaveLoad.load(file).get
    val ws2 = gc2.state
    ws2.regions.size shouldBe ws.regions.size
  }
}
