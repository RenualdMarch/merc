package mr.merc.ui.common

import mr.merc.ai.BattleAI
import mr.merc.economics.WorldGenerationConstants.WorldMapCreationConf
import mr.merc.economics.{WorldGenerator, WorldState}
import mr.merc.game.QuickGameGenerator
import scalafx.Includes._
import scalafx.stage.Stage
import scalafx.scene.Parent
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.menu.MainMenu
import mr.merc.log.Logging
import mr.merc.ui.menu.OptionsMenu
import mr.merc.ui.world.{CustomNewGameFrame, WorldFrame}
import scalafx.scene.layout.Pane

class SceneManager(val stage: Stage) extends Logging {

  def startNewBattle(callbackFunction:() => Unit) {
    info("starting new battle")
    val gameField = new QuickGameGenerator().generateGame
    val map = Map(gameField.players.find(_.name.contains("AI")).get -> BattleAI())
    sceneRoot = new BattleFrame(this, gameField, map, callbackFunction)
  }

  def startNewWorld(): Unit = {
    info("starting quick game")
    val worldState = WorldGenerator.generateWorld(WorldMapCreationConf(50, 50, 100))
    sceneRoot = new WorldFrame(this, worldState)
  }

  def startCustomNewWorldDialog(): Unit = {
    info("Custom new world dialog")
    sceneRoot = new CustomNewGameFrame(this)
  }

  def startCustomNewWorld(worldState: WorldState): Unit = {
    info("starting new world")
    sceneRoot = new WorldFrame(this, worldState)
  }

  def showMainMenu() {
    info("showing main menu")
    sceneRoot = new MainMenu(this)
  }

  def showOptionsMenu() {
    info("showing options menu")
    sceneRoot = new OptionsMenu(this)
  }

  def showFrame(pane:Pane): Unit = {
    info(s"showing frame $pane")
    sceneRoot = pane
  }

  def exit() {
    info("exit")
    stage.close()
  }

  private def sceneRoot_=(parent: Parent) {
    stage.scene.value.root = parent
  }

  private def sceneRoot = stage.scene.value.root
}