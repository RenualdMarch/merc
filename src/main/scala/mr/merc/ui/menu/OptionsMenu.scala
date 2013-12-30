package mr.merc.ui.menu

import scalafx.scene.layout.GridPane
import scalafx.scene.layout.VBox
import scalafx.scene.layout.HBox
import scalafx.scene.control.Button
import mr.merc.local.Localization
import scalafx.scene.control.CheckBox
import scalafx.scene.control.ComboBox
import scalafx.geometry.Pos._
import scalafx.scene.text.Text
import mr.merc.conf.Conf
import mr.merc.ui.common.ConversionUtils._
import javafx.event.ActionEvent
import mr.merc.ui.common.SceneManager
import scalafx.scene.layout.BorderPane
import javafx.geometry.Pos
import scalafx.scene.layout.Priority
import scalafx.geometry.Insets

class OptionsMenu(sceneManager: SceneManager) extends BorderPane {
  styleClass.add("optionsMenuContainer")
  stylesheets.add("/css/optionsMenu.css")

  private val soundEnabled = new CheckBox
  private val musicEnabled = new CheckBox
  private val language = new ComboBox(Localization.languages)

  fromConfigToComponents()
  val controlsPanel = createControlsPanel
  val buttonsPanel = createButtonsPanel
  val centerBox = new VBox {
    styleClass.add("optionsPane")
    content = List(controlsPanel, buttonsPanel)
    spacing = 20
  }
  centerBox.width.onChange {
    centerBox.padding = Insets(centerBox.height.value / 4, centerBox.width.value / 4, centerBox.height.value / 4, centerBox.width.value / 3)
  }
  centerBox.height.onChange {
    centerBox.padding = Insets(centerBox.height.value / 4, centerBox.width.value / 4, centerBox.height.value / 4, centerBox.width.value / 3)
  }
  centerBox.maxWidth <== width / 4
  centerBox.maxHeight <== height / 3
  center = centerBox

  private def createControlsPanel: GridPane = {
    new GridPane {
      styleClass.add("optionsTable")
      vgap = 20
      hgap = 10

      add(new Text {
        text = Localization("options.soundsEnabled")
        styleClass.add("textLabels")
      }, 0, 0)
      add(soundEnabled, 1, 0)
      add(new Text {
        text = Localization("options.musicEnabled")
        styleClass.add("textLabels")
      }, 0, 1)
      add(musicEnabled, 1, 1)
      add(new Text {
        text = Localization("options.language")
        styleClass.add("textLabels")
      }, 0, 2)
      add(language, 1, 2)
    }
  }

  private def createButtonsPanel: HBox = {
    val okButton = new Button {
      text = Localization("common.ok")
      onAction = { e: ActionEvent =>
        fromComponentsToConfig()
        sceneManager.showMainMenu()
      }
    }

    val cancelButton = new Button {
      text = Localization("common.cancel")
      onAction = { e: ActionEvent =>
        sceneManager.showMainMenu()
      }
    }

    new HBox {
      content = List(okButton, cancelButton)
    }
  }

  private def fromComponentsToConfig() {
    val map = Map("Sound" -> soundEnabled.selected.value.toString,
      "Music" -> musicEnabled.selected.value.toString, "Language" -> language.value.value)
    Conf.writeChanges(map, false, true)
  }

  private def fromConfigToComponents() {
    soundEnabled.selected = Conf.bool("Sound")
    musicEnabled.selected = Conf.bool("Music")
    language.value.value = Conf.string("Language")
  }
}