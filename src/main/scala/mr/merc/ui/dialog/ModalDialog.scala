package mr.merc.ui.dialog

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

}

