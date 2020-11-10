package mr.merc.music

import java.io.File

import scala.util.Random
import mr.merc.sound.Sound
import mr.merc.conf.Conf
import scalafx.scene.media.MediaPlayer.Status

object MusicPlayer {

  private val pathPrefix = "/music/"
  val trackList = List("Spring_Mvt_1_Allegro.mp3", "Summer_Mvt_3_Presto.mp3",
    "Autumn_Mvt_1_Allegro.mp3", "Winter_Mvt_1_Allegro_non_molto.mp3")

  private def randomSong: String = pathPrefix + trackList(Random.nextInt(trackList.size))

  def playMusic() {
    if (Conf.bool("Music")) {
      new Sound(randomSong, playMusic).play()
    }
  }
}