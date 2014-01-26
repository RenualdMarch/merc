package mr.merc.sound

import scala.concurrent._
import ExecutionContext.Implicits.global
import mr.merc.conf.Conf

object SoundPlayer {
  def playSound(path: String, s: Status => Unit) {
    if (Conf.bool("Sound")) {
      future {
        renderer(path, s).play(getClass.getResourceAsStream(path))
      }
    }
  }

  private def renderer(path: String, s: Status => Unit): BaseAudioRenderer = {
    val lowercase = path.toLowerCase
    if (lowercase.endsWith("ogg")) {
      new JOrbisOggRenderer(s)
    } else if (lowercase.endsWith("mp3")) {
      new JavaLayerMp3Renderer(s)
    } else {
      throw new IllegalArgumentException("Renderer for path " + path + " is not found!")
    }
  }

}

