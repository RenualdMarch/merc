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
import scalafx.event.ActionEvent

class BattleResultDialog(result: BattleResult, sceneManager: SceneManager) extends Stage {
  title = Localization("attack.battleResult.title")

  val line = result.winningAlliance.map(_.name).mkString(", ")
  val label = new Text {
    text = Localization("attack.battleResult.message", line)
  }

  val okButton = new Button {
    text = Localization("common.ok")
    onAction = { e: ActionEvent =>
      BattleResultDialog.this.close()
      sceneManager.showMainMenu
    }
  }
  this.scene = new Scene {
    content = new VBox {
      children = List(label, okButton)
    }
  }
}