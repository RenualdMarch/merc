package mr.merc.ui.battle

import scalafx.stage.Stage
import scalafx.Includes._
import mr.merc.local.Localization
import scalafx.scene.layout.VBox
import mr.merc.ui.common.SceneManager
import mr.merc.battle.BattleResult
import mr.merc.ui.world.BigText
import scalafx.scene.control.Button
import scalafx.scene.Scene

class BattleResultDialog(result: BattleResult, sceneManager: SceneManager, battleOverCallback:() => Unit) extends Stage {
  title = Localization("attack.battleResult.title")

  val line = result.winningAlliance.map(_.name).mkString(", ")
  val label = BigText(Localization("attack.battleResult.message", line))

  val okButton = new Button {
    text = Localization("common.ok")
    onAction = { _ =>
      BattleResultDialog.this.close()
      battleOverCallback()
    }
  }
  this.scene = new Scene {
    content = new VBox {
      children = List(label, okButton)
    }
  }
}