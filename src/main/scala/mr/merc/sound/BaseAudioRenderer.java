package mr.merc.sound;

import java.net.URL;

// TODO rewrite this class into scala
public abstract class BaseAudioRenderer {

	public enum Status {
		PLAYING, STOPPED, END_OF_SOUND, ERROR
	}

	private AudioEventsListener audioEventsListener;

	private Status status;

	public BaseAudioRenderer() {
		setStatus(Status.STOPPED);
	}

	protected abstract void playSound(URL audiofile);

	public void play(URL audiofile) {
		setStatus(Status.PLAYING);
		playSound(audiofile);
	}

	public AudioEventsListener getAudioEventsListener() {
		return audioEventsListener;
	}

	public void setAudioEventsListener(AudioEventsListener audioEventsListener) {
		this.audioEventsListener = audioEventsListener;
	}

	public Status getStatus() {
		return status;
	}

	protected void setStatus(Status status) {
		if (this.status != status) {
			this.status = status;

			if (getAudioEventsListener() != null) {
				getAudioEventsListener().notifyStatusChange(status);
				if (status == Status.END_OF_SOUND || status == Status.ERROR) {
					setAudioEventsListener(null);
				}
			}
		}
	}
}
