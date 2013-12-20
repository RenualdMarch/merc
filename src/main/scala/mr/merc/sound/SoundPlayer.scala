package mr.merc.sound

import scala.concurrent._
import ExecutionContext.Implicits.global

object SoundPlayer {
  private val soundsEnabled = true

  def playSound(path: String, s: BaseAudioRenderer.Status => Unit): AudioEventsListener = {
    if (soundsEnabled) {
      val r = renderer(path)
      r.setAudioEventsListener(new AudioEventsListener {
        def apply(newStatus: BaseAudioRenderer.Status) = {
          s(newStatus)
        }
      })
      future {
        r.play(getClass.getResource(path))
      }
      r.getAudioEventsListener()
    } else {
      null
    }
  }

  private def renderer(path: String): BaseAudioRenderer = {
    val lowercase = path.toLowerCase
    if (lowercase.endsWith("wav")) {
      new WaveRenderer
    } else if (lowercase.endsWith("ogg")) {
      new JOrbisOggRenderer
    } else if (lowercase.endsWith("mp3")) {
      new JavaLayerMp3Renderer
    } else {
      throw new IllegalArgumentException("Renderer for path " + path + " is not found!")
    }
  }

}

