package mr.merc.ui.battle

import scalafx.stage.Stage
import scalafx.Includes._
import mr.merc.local.Localization
import scalafx.scene.layout.VBox
import mr.merc.ui.common.SceneManager
import mr.merc.battle.BattleResult
import scalafx.scene.text.Text
import scalafx.scene.control.Button
import scalafx.scene.Scene

class BattleResultDialog(result: BattleResult, sceneManager: SceneManager) extends Stage {
  title = Localization("attack.battleResult.title")

  val label = new Text {
    text = Localization("attack.battleResult.message", result.winningAlliance.mkString(", "))
  }

  val okButton = new Button {
    text = Localization("common.ok")
    onAction = {
      BattleResultDialog.this.close()
      sceneManager.showMainMenu
    }
  }

  val vbox = new VBox {
    content = List(label, okButton)
  }

  this.scene = new Scene {
    content = vbox
  }
}