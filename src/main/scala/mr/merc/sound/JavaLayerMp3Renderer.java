package mr.merc.sound;

//JFC
import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.URL;

import javazoom.jl.decoder.JavaLayerException;
import javazoom.jl.player.FactoryRegistry;
import javazoom.jl.player.Player;
import javazoom.jl.player.advanced.AdvancedPlayer;
import javazoom.jl.player.advanced.PlaybackEvent;
import javazoom.jl.player.advanced.PlaybackListener;

// TODO rewrite into scala
public class JavaLayerMp3Renderer extends BaseAudioRenderer {

	private AdvancedPlayer player;

	@Override
	protected void playSound(final URL audiofile) {
		try {
			player = new AdvancedPlayer(new BufferedInputStream(
					audiofile.openStream()), FactoryRegistry.systemRegistry()
					.createAudioDevice());

			player.setPlayBackListener(new PlaybackListener() {
				@Override
				public void playbackFinished(PlaybackEvent pe) {
					super.playbackFinished(pe);
					setStatus(Status.END_OF_SOUND);
				}
			});
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
}
