package mr.merc.sound

import java.net.URL
import javax.sound.sampled.Clip
import javax.sound.sampled.AudioSystem
import javax.sound.sampled.Line
import javax.sound.sampled.LineListener
import javax.sound.sampled.LineEvent

class WavRenderer(listener: Status => Unit) extends BaseAudioRenderer(listener) {

  def playSound(audiofile: URL) {
    val clip = AudioSystem.getLine(new Line.Info(classOf[Clip])).asInstanceOf[Clip]

    clip.addLineListener(new LineListener() {
      def update(event: LineEvent) {
        if (event.getType() == LineEvent.Type.STOP) {
          clip.close()
          status = EndOfSound
        }
      }
    })

    clip.open(AudioSystem.getAudioInputStream(audiofile))
    clip.drain()
    clip.start()
  }
}