package mr.merc.sound

import java.net.URL
import scala.beans.BeanProperty

abstract class BaseAudioRenderer(listener: Status => Unit) {
  private var _status: Status = Stopped
  def status = _status

  def status_=(st: Status) {
    _status = st
    listener(st)
  }

  def playSound(audiofile: URL)

  def play(audiofile: URL) {
    status = Playing
    playSound(audiofile)
  }
}

sealed trait Status
object Playing extends Status
object Stopped extends Status
object EndOfSound extends Status
object SoundError extends Status

