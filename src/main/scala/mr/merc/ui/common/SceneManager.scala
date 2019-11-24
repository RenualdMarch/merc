package mr.merc.ui.common

import mr.merc.ai.BattleAI
import mr.merc.economics.WorldGenerator
import mr.merc.game.QuickGameGenerator
import scalafx.Includes._
import scalafx.stage.Stage
import scalafx.scene.Parent
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.menu.MainMenu
import mr.merc.log.Logging
import mr.merc.ui.menu.OptionsMenu
import mr.merc.ui.world.WorldFrame

class SceneManager(val stage: Stage) extends Logging {

  def startNewBattle() {
    info("starting new battle")
    val gameField = new QuickGameGenerator().generateGame
    val map = Map(gameField.players.find(_.name.contains("AI")).get -> BattleAI())
    sceneRoot = new BattleFrame(this, gameField, map)
  }

  def startNewWorld(): Unit = {
    val worldState = WorldGenerator.generateWorld()
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

  def exit() {
    info("exit")
    stage.close()
  }

  private def sceneRoot_=(parent: Parent) {
    stage.scene.value.root = parent
  }

  private def sceneRoot = stage.scene.value.root
}