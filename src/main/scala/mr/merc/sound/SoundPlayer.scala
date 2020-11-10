package mr.merc.sound

import mr.merc.conf.Conf
import scalafx.scene.media.{Media, MediaPlayer}

object SoundPlayer {

  private var cache:List[MediaPlayer] = Nil

  def playSound(path: String, onStop: () => Unit): Option[MediaPlayer] = {
    if (Conf.bool("Sound")) {
      val sound = new Media(getClass.getResource(path).toURI.toString)
      val player = new MediaPlayer(sound)

      player.onEndOfMedia = new Runnable {
        override def run(): Unit = {
          onStop()
          player.dispose()
          cache = cache.filterNot(_ == player)
        }
      }

      player.autoPlay = true

      cache = player :: cache
      Some(player)
    } else None
  }
}

