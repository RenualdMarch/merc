package mr.merc.sound

class Sound(val path: String, s: () => Unit = () => ()) {
  def play() {
    SoundPlayer.playSound(path, s)
  }
}
