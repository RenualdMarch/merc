package mr.merc.ui.dialog

import scalafx.scene.{Node, Scene}
import scalafx.stage.{Modality, Stage}

object ModalDialog {

  implicit class ShowDialog[P <: Stage](dialog: P) {
    def showDialog(owner: Stage): P = {
      dialog.initModality(Modality.WindowModal)
      dialog.initOwner(owner)
      dialog.centerOnScreen()
      dialog.showAndWait()
      dialog
    }
  }

  implicit class ShowInfoDialog(dialog: Node) {
    def showDialog(owner: Stage): Stage = {
      val stage = new Stage {
        this.scene = new Scene {
          content = dialog
        }
      }

      stage.initModality(Modality.WindowModal)
      stage.initOwner(owner)
      stage.centerOnScreen()
      stage.showAndWait()
      stage
    }
  }

  implicit class ShowInfoDialogJava(dialog: javafx.scene.Node) {

    def showDialog(owner: Stage): Stage = {
      import scalafx.Includes._

      val stage = new Stage {
        this.scene = new Scene {
          content = dialog
        }
      }

      stage.initModality(Modality.WindowModal)
      stage.initOwner(owner)
      stage.centerOnScreen()
      stage.showAndWait()
      stage
    }
  }

}

