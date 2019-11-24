package mr.merc.ui.dialog

import scalafx.scene.Scene
import scalafx.scene.control.ProgressIndicator
import scalafx.stage.{Stage, StageStyle}

class WaitDialog extends Stage {
  scene = new Scene {
    content = new ProgressIndicator {
      stylesheets.add("/css/waitDialog.css")
      minWidth = 300
      minHeight = 200
    }
  }

  this.initStyle(StageStyle.Undecorated)

}
