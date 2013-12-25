package mr.merc.sound

import java.net.URL
import java.io.InputStream

class JOrbisOggRenderer(listener: Status => Unit) extends BaseAudioRenderer(listener) {
  def setStatus(st: Status) = {
    status = st
  }

  def getStatus = status

  def playSound(audiofile: InputStream) {
    val base = new JOrbisOggRendererBase(this)
    base.playSound(audiofile)
  }
}