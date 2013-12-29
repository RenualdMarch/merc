package mr.merc.ui.common

import javafx.event.EventHandler
import javafx.event.Event

object ConversionUtils {
  implicit def func2eventHandler[T <: Event](f: T => Unit) = new EventHandler[T] {
    def handle(event: T) = f(event)
  }
}