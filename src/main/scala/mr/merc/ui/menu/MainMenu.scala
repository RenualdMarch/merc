package mr.merc.ui.menu

import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.VBox
import scalafx.geometry.Insets
import scalafx.scene.control.Button
import mr.merc.local.Localization
import scalafx.geometry.Pos
import mr.merc.ui.common.ConversionUtils._
import javafx.event.ActionEvent
import scalafx.scene.Scene
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.common.SceneManager

class MainMenu(sceneManager: SceneManager) extends BorderPane {

  styleClass.add("mainMenuContainer")
  stylesheets.add("/css/mainMenu.css")

  val newBattleButton = new Button {
    text = Localization("menu.newBattle")
    onAction = { e: ActionEvent =>
      sceneManager.startNewBattle()
    }
  }

  val optionsButton = new Button {
    text = Localization("menu.options")
    onAction = { e: ActionEvent =>
      sceneManager.showOptionsMenu()
    }
  }

  val exitButton = new Button {
    text = Localization("menu.exit")
    onAction = { e: ActionEvent =>
      sceneManager.exit()
    }
  }

  val menuPane = new VBox {
    content = List(newBattleButton, optionsButton, exitButton)
  }

  menuPane.styleClass.add("menuPane")

  menuPane.spacing <== height / (5 * menuPane.children.size())

  menuPane.maxWidth <== width / 5
  menuPane.maxHeight <== height / 2
  menuPane.minHeight <== height / 2
  menuPane.alignment = Pos.CENTER

  bottom = menuPane

  BorderPane.setAlignment(menuPane, Pos.BOTTOM_CENTER)
}