package mr.merc.ui.common

import mr.merc.log.Logging
import scalafx.application.Platform
import scalafx.scene.Node
import scalafx.scene.control.ProgressIndicator
import scalafx.scene.layout.BorderPane

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}

class LazyPane(f: => Node) extends BorderPane with Logging {

  private val progressIndicator = new ProgressIndicator {
    stylesheets.add("/css/waitDialog.css")
    minWidth <== this.width
    minHeight <== this.height
  }
  this.center = progressIndicator

  Future(f).onComplete {
    case Failure(exception) => error(exception.getMessage, exception)
    case Success(value) => Platform.runLater {
      this.center = value
    }
  }

}
