package mr.merc.sound

import java.net.URL

class JOrbisOggRenderer(listener: Status => Unit) extends BaseAudioRenderer(listener) {
  def setStatus(st: Status) = {
    status = st
  }

  def getStatus = status

  def playSound(audiofile: URL) {
    val base = new JOrbisOggRendererBase(this)
    base.playSound(audiofile)
  }
}