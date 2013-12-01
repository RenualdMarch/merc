package mr.merc.sound;

//JFC
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;

import javazoom.jl.decoder.JavaLayerException;
import javazoom.jl.player.FactoryRegistry;
import javazoom.jl.player.Player;

public class JavaLayerMp3Renderer extends BaseAudioRenderer {

	private Player player;

	@Override
	protected void playSound(final URL audiofile) {
		try {
			player = new Player(
					new BufferedInputStream(audiofile.openStream()),
					FactoryRegistry.systemRegistry().createAudioDevice());

			player.play();
		} catch (IOException e) {
			setStatus(Status.ERROR);
			System.err.println("Can not load audiofile (" + audiofile + ": "
					+ e);
		} catch (JavaLayerException e) {
			setStatus(Status.ERROR);
			System.err.println("Problem playing audio: " + e);
		}
	}

	@Override
	public Status getStatus() {
		if (player != null) {
			// return EOS if the sound has been completed played
			return (player.isComplete()) ? Status.END_OF_SOUND : super
					.getStatus();
		}

		return super.getStatus();
	}
}
