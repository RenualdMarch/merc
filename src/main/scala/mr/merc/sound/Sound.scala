package mr.merc.sound

class Sound(val path: String, s: BaseAudioRenderer.Status => Unit = s => {}) {
  def play() {
    SoundPlayer.playSound(path, s)
  }
}
