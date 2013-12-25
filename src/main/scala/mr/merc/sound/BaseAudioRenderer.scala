package mr.merc.sound

import java.net.URL
import scala.beans.BeanProperty
import java.io.InputStream

abstract class BaseAudioRenderer(listener: Status => Unit) {
  private var _status: Status = Stopped
  def status = _status

  def status_=(st: Status) {
    _status = st
    listener(st)
  }

  def playSound(audiofile: InputStream)

  def play(stream: InputStream) {
    status = Playing
    playSound(stream)
  }
}

sealed trait Status
object Playing extends Status
object Stopped extends Status
object EndOfSound extends Status
object SoundError extends Status

