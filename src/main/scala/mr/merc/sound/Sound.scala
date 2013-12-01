package mr.merc.sound

class Sound(val path:String) {
  def play() {
    SoundPlayer.playSound(path)
  }
}
