package mr.merc.ui.common

import scalafx.Includes._
import scalafx.stage.Stage
import scalafx.scene.Parent
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.menu.MainMenu
import mr.merc.log.Logging
import mr.merc.ui.menu.OptionsMenu
import mr.merc.ui.world.WorldFrame

class SceneManager(val stage: Stage) extends Logging {

  def startNewGame() {
    info("starting new game")
    sceneRoot = new WorldFrame(this)
  }

  def startNewBattle() {
    info("starting new battle")
    sceneRoot = new BattleFrame(this)
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