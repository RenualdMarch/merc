package mr.merc.ui.common

import scalafx.Includes._
import scalafx.stage.Stage
import scalafx.scene.Parent
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.menu.MainMenu
import mr.merc.log.Logging

class SceneManager(val stage: Stage) extends Logging {

  def startNewBattle() {
    info("starting new battle")
    sceneRoot = new BattleFrame
  }

  def showMainMenu() {
    info("showing main menu")
    sceneRoot = new MainMenu(this)
  }

  private def sceneRoot_=(parent: Parent) {
    stage.scene.value.root = parent
  }

  private def sceneRoot = stage.scene.value.root
}