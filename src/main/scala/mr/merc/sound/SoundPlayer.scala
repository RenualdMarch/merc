package mr.merc.sound

import scala.concurrent._
import ExecutionContext.Implicits.global

object SoundPlayer {
  private val soundsEnabled = true

  def playSound(path: String) {
    if (soundsEnabled) {
      future {
        renderer(path).play(getClass.getResource(path))
      }
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

