package mr.merc.music

import java.io.File
import scala.util.Random
import mr.merc.sound.Sound
import mr.merc.sound.BaseAudioRenderer
import mr.merc.conf.Conf

object MusicPlayer {

  private val pathPrefix = "/music/"
  val trackList = List("Spring_Mvt_1_Allegro.mp3", "Summer_Mvt_3_Presto.mp3",
    "Autumn_Mvt_1_Allegro.mp3", "Winter_Mvt_1_Allegro_non_molto.mp3")

  private var _musicPlaying = false
  def musicPlaying = _musicPlaying
  private def randomSong: String = pathPrefix + trackList(Random.nextInt(trackList.size))

  def playMusic() {
    if (!Conf.bool("Music")) {
      _musicPlaying = false
      return ;
    }

    _musicPlaying = true
    new Sound(randomSong, s => {
      if (s == BaseAudioRenderer.Status.END_OF_SOUND) {
        playMusic()
      }
    }).play()
  }
}