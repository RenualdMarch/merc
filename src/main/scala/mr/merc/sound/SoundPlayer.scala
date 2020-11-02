package mr.merc.sound

import mr.merc.conf.Conf
import scalafx.scene.media.MediaPlayer.Status
import scalafx.scene.media.{Media, MediaPlayer}

object SoundPlayer {

  private var cache:List[MediaPlayer] = Nil

  def playSound(path: String, s: Status => Unit): Option[MediaPlayer] = {
    if (Conf.bool("Sound")) {
      val sound = new Media(getClass.getResource(path).toURI.toString)

      val (finished, notFinished) = cache.partition(_.status.value == MediaPlayer.Status.Stopped.delegate)
      val player = new MediaPlayer(sound)

      player.status.onChange {
        s(scalafx.scene.media.MediaPlayer.Status(player.status.value))
      }
      player.autoPlay = true
      finished.foreach(_.dispose())

      cache = player :: notFinished
      Some(player)
    } else None
  }
}

