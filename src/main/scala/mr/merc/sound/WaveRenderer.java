package mr.merc.sound;


//JFC
import java.net.URL;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Clip;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.Mixer;

/**
* Play wave sound (*.wav, *.au).
*/
public class WaveRenderer extends BaseAudioRenderer implements LineListener {

     private Clip clip;
     private Mixer mixer; // workaround for Java 1.5

     public WaveRenderer() {
     for (Mixer.Info mixer : AudioSystem.getMixerInfo()) {
         if ("Java Sound Audio Engine".equals(mixer.getName())) {
             this.mixer = AudioSystem.getMixer(mixer);
         }
     }

     }

     @Override
     protected void playSound(URL audiofile) {
             try {
                     if (clip != null) {
                             clip.drain();
                             clip.close();
                     }

                     AudioInputStream ain = AudioSystem.getAudioInputStream(audiofile);
                     AudioFormat format = ain.getFormat();

                     if ((format.getEncoding() == AudioFormat.Encoding.ULAW)
                                     || (format.getEncoding() == AudioFormat.Encoding.ALAW)) {
                             // we can't yet open the device for ALAW/ULAW playback,
                             // convert ALAW/ULAW to PCM
                             AudioFormat temp = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED, format.getSampleRate(),
                                             format.getSampleSizeInBits() * 2, format.getChannels(), format.getFrameSize() * 2,
                                             format.getFrameRate(), true);
                             ain = AudioSystem.getAudioInputStream(temp, ain);
                             format = temp;
                     }

                     DataLine.Info info = new DataLine.Info(Clip.class, ain.getFormat(),
                                     ((int) ain.getFrameLength() * format.getFrameSize()));
                     // workaround for Java 1.5
                     if (mixer != null) {
                             clip = (Clip) mixer.getLine(info);

                     } else {
                             clip = (Clip) AudioSystem.getLine(info);
                     }
                     // clip = (Clip) AudioSystem.getLine(info);
                     clip.addLineListener(this);

                     clip.open(ain);
                     clip.start();

             } catch (Exception e) {
                     setStatus(Status.ERROR);
                     System.err.println("ERROR: Can not playing " + audiofile + " caused by: " + e);
             }
     }

     @Override
     public void update(LineEvent e) {
             if (e.getType() == LineEvent.Type.STOP) {
                     setStatus(Status.END_OF_SOUND);
                     clip.stop();
                     clip.setMicrosecondPosition(0);
                     clip.removeLineListener(this);
             }
     }
}


