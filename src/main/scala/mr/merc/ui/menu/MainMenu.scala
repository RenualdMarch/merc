package mr.merc.ui.menu

import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import mr.merc.local.Localization
import scalafx.geometry.Pos
import javafx.event.ActionEvent
import mr.merc.log.Logging
import mr.merc.ui.common.SceneManager
import mr.merc.ui.world.BigButton

class MainMenu(sceneManager: SceneManager) extends BorderPane with Logging {

  styleClass.add("mainMenuContainer")
  stylesheets.add("/css/mainMenu.css")

  val newBattleButton = new BigButton() {
    text = Localization("menu.quickBattle")
    onAction = { e: ActionEvent =>
      sceneManager.startNewBattle(() => sceneManager.showMainMenu())
    }
  }

  val newWorldButton = new BigButton() {
    text = Localization("menu.quickGame")
    onAction = { e: ActionEvent =>
      sceneManager.startNewWorld()
    }
  }

  val customNewWorldButton = new BigButton {
    text = Localization("menu.customGame")
    onAction = { _ =>
      sceneManager.startCustomNewWorldDialog()
    }
  }

  val optionsButton = new BigButton() {
    text = Localization("menu.options")
    onAction = { e: ActionEvent =>
      sceneManager.showOptionsMenu()
    }
  }

  val exitButton = new BigButton() {
    text = Localization("menu.exit")
    onAction = { e: ActionEvent =>
      sceneManager.exit()
    }
  }

  val loadButton = new BigButton() {
    text = Localization("menu.load")
    onAction = { _ =>
      sceneManager.loadGame()
    }
  }

  val buttons = List(newBattleButton, newWorldButton, customNewWorldButton, loadButton, optionsButton, exitButton)

  val menuPane = new VBox {
    children = buttons
  }

  buttons.foreach { b =>
    b.prefWidth <== menuPane.width / 2
  }

  menuPane.styleClass.add("menuPane")

  menuPane.spacing <== height / 60

  menuPane.maxWidth <== width / 5
  menuPane.maxHeight <== height / 2
  menuPane.minHeight <== height / 2
  menuPane.alignment = Pos.Center

  bottom = menuPane

  BorderPane.setAlignment(menuPane, Pos.BottomCenter)
}