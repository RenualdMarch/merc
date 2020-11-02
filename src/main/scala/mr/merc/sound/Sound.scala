package mr.merc.sound

import scalafx.scene.media.MediaPlayer.Status

class Sound(val path: String, s: Status => Unit = s => ()) {
  def play() {
    SoundPlayer.playSound(path, s)
  }
}
