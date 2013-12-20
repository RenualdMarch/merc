package mr.merc.sound

import java.net.URL
import javazoom.jl.player.advanced.AdvancedPlayer
import java.io.BufferedInputStream
import javazoom.jl.player.FactoryRegistry
import javazoom.jl.player.advanced.PlaybackListener
import javazoom.jl.player.advanced.PlaybackEvent

class JavaLayerMp3Renderer(listener: Status => Unit) extends BaseAudioRenderer(listener) {

  def playSound(audiofile: URL) {
    val player = new AdvancedPlayer(new BufferedInputStream(
      audiofile.openStream()), FactoryRegistry.systemRegistry()
      .createAudioDevice())

    player.setPlayBackListener(new PlaybackListener() {
      override def playbackFinished(pe: PlaybackEvent) {
        super.playbackFinished(pe)
        status = EndOfSound
      }
    })
    player.play()
  }
}