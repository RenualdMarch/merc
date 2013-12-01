package mr.merc.sound;

//JFC
import java.io.InputStream;
import java.net.URL;

import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.SourceDataLine;

import com.jcraft.jogg.Packet;
import com.jcraft.jogg.Page;
import com.jcraft.jogg.StreamState;
import com.jcraft.jogg.SyncState;
import com.jcraft.jorbis.Block;
import com.jcraft.jorbis.Comment;
import com.jcraft.jorbis.DspState;
import com.jcraft.jorbis.Info;

public class JOrbisOggRenderer extends BaseAudioRenderer {

	private static final int BUFSIZE = 4096 * 2;

	private static int convsize = JOrbisOggRenderer.BUFSIZE * 2;
	private static byte[] convbuffer = new byte[JOrbisOggRenderer.convsize];

	private SyncState oy;
	private StreamState os;
	private Page og;
	private Packet op;
	private Info vi;
	private Comment vc;
	private DspState vd;
	private Block vb;

	private byte[] buffer;
	private int bytes;

	private int rate;
	private int channels;
	private SourceDataLine outputLine;

	private InputStream bitStream;

	@Override
	protected void playSound(URL audiofile) {
		try {
			bitStream = audiofile.openStream();
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}

		run();
	}

	private SourceDataLine getOutputLine(int channels, int rate) {
		if (outputLine != null || this.rate != rate
				|| this.channels != channels) {
			if (outputLine != null) {
				outputLine.drain();
				outputLine.stop();
				outputLine.close();
			}

			init_audio(channels, rate);
			outputLine.start();
		}
		return outputLine;
	}

	private void init_audio(int channels, int rate) {
		try {
			AudioFormat audioFormat = new AudioFormat(rate, 16, channels, true, // PCM_Signed
					false // littleEndian
			);
			DataLine.Info info = new DataLine.Info(SourceDataLine.class,
					audioFormat, AudioSystem.NOT_SPECIFIED);
			if (!AudioSystem.isLineSupported(info)) {
				System.out.println("Line " + info + " not supported.");
				return;
			}

			try {
				outputLine = (SourceDataLine) AudioSystem.getLine(info);
				// outputLine.addLineListener(this);
				outputLine.open(audioFormat);
			} catch (LineUnavailableException e) {
				System.out.println("Unable to open the sourceDataLine: " + e);
				return;
			} catch (IllegalArgumentException e) {
				System.out.println("Illegal Argument: " + e);
				return;
			}

			this.rate = rate;
			this.channels = channels;
		} catch (Exception e) {
			System.out.println(e);
		}
	}

	private void init_jorbis() {
		oy = new SyncState();
		os = new StreamState();
		og = new Page();
		op = new Packet();

		vi = new Info();
		vc = new Comment();
		vd = new DspState();
		vb = new Block(vd);

		buffer = null;
		bytes = 0;

		oy.init();
	}

	public final void run() {
		init_jorbis();

		loop: while (true) {
			int eos = 0;

			int index = oy.buffer(JOrbisOggRenderer.BUFSIZE);
			buffer = oy.data;
			try {
				bytes = bitStream
						.read(buffer, index, JOrbisOggRenderer.BUFSIZE);
			} catch (Exception e) {
				setStatus(Status.ERROR);
				System.err.println(e);
				return;
			}
			oy.wrote(bytes);

			if (oy.pageout(og) != 1) {
				if (bytes < JOrbisOggRenderer.BUFSIZE) {
					break;
				}
				setStatus(Status.ERROR);
				System.err
						.println("Input does not appear to be an Ogg bitstream.");
				return;
			}

			os.init(og.serialno());
			os.reset();

			vi.init();
			vc.init();

			if (os.pagein(og) < 0) {
				// error; stream version mismatch perhaps
				setStatus(Status.ERROR);
				System.err
						.println("Error reading first page of Ogg bitstream data.");
				return;
			}

			if (os.packetout(op) != 1) {
				// no page? must not be vorbis
				setStatus(Status.ERROR);
				System.err.println("Error reading initial header packet.");
				break;
				// return;
			}

			if (vi.synthesis_headerin(vc, op) < 0) {
				// error case; not a vorbis header
				setStatus(Status.ERROR);
				System.err
						.println("This Ogg bitstream does not contain Vorbis audio data.");
				return;
			}

			int i = 0;

			while (i < 2) {
				while (i < 2) {
					int result = oy.pageout(og);
					if (result == 0) {
						break; // Need more data
					}
					if (result == 1) {
						os.pagein(og);
						while (i < 2) {
							result = os.packetout(op);
							if (result == 0) {
								break;
							}
							if (result == -1) {
								setStatus(Status.ERROR);
								System.err
										.println("Corrupt secondary header.  Exiting.");
								// return;
								break loop;
							}
							vi.synthesis_headerin(vc, op);
							i++;
						}
					}
				}

				index = oy.buffer(JOrbisOggRenderer.BUFSIZE);
				buffer = oy.data;
				try {
					bytes = bitStream.read(buffer, index,
							JOrbisOggRenderer.BUFSIZE);
				} catch (Exception e) {
					setStatus(Status.ERROR);
					System.err.println(e);
					return;
				}

				if (bytes == 0 && i < 2) {
					setStatus(Status.ERROR);
					System.err
							.println("End of file before finding all Vorbis headers!");
					return;
				}

				oy.wrote(bytes);
			}

			JOrbisOggRenderer.convsize = JOrbisOggRenderer.BUFSIZE
					/ vi.channels;

			vd.synthesis_init(vi);
			vb.init(vd);

			float[][][] _pcmf = new float[1][][];
			int[] _index = new int[vi.channels];

			getOutputLine(vi.channels, vi.rate);

			while (eos == 0) {
				while (eos == 0) {

					if (getStatus() != Status.PLAYING) {
						try {
							// outputLine.drain();
							// outputLine.stop();
							// outputLine.close();
							bitStream.close();
						} catch (Exception e) {
							System.err.println(e);
						}

						return;
					}

					int result = oy.pageout(og);
					if (result == 0) {
						break; // need more data
					}
					if (result == -1) { // missing or corrupt data at this
						// page position
						// System.err.println("Corrupt or missing data in
						// bitstream; continuing...");
					} else {
						os.pagein(og);
						while (true) {
							result = os.packetout(op);
							if (result == 0) {
								break; // need more data
							}
							if (result == -1) { // missing or corrupt data
								// at this page position
								// no reason to complain; already complained
								// above
							} else {
								// we have a packet. Decode it
								int samples;
								if (vb.synthesis(op) == 0) { // test
									// for
									// success!
									vd.synthesis_blockin(vb);
								}
								while ((samples = vd.synthesis_pcmout(_pcmf,
										_index)) > 0) {
									float[][] pcmf = _pcmf[0];
									int bout = (samples < JOrbisOggRenderer.convsize ? samples
											: JOrbisOggRenderer.convsize);

									// convert doubles to 16 bit signed ints
									// (host order) and
									// interleave
									for (i = 0; i < vi.channels; i++) {
										int ptr = i * 2;
										// int ptr=i;
										int mono = _index[i];
										for (int j = 0; j < bout; j++) {
											int val = (int) (pcmf[i][mono + j] * 32767.);
											if (val > 32767) {
												val = 32767;
											}
											if (val < -32768) {
												val = -32768;
											}
											if (val < 0) {
												val = val | 0x8000;
											}
											JOrbisOggRenderer.convbuffer[ptr] = (byte) (val);
											JOrbisOggRenderer.convbuffer[ptr + 1] = (byte) (val >>> 8);
											ptr += 2 * (vi.channels);
										}
									}
									outputLine.write(
											JOrbisOggRenderer.convbuffer, 0, 2
													* vi.channels * bout);
									vd.synthesis_read(bout);
								}
							}
						}
						if (og.eos() != 0) {
							eos = 1;
						}
					}
				}

				if (eos == 0) {
					index = oy.buffer(JOrbisOggRenderer.BUFSIZE);
					buffer = oy.data;
					try {
						bytes = bitStream.read(buffer, index,
								JOrbisOggRenderer.BUFSIZE);
					} catch (Exception e) {
						setStatus(Status.ERROR);
						System.err.println(e);
						return;
					}
					if (bytes == -1) {
						break;
					}
					oy.wrote(bytes);
					if (bytes == 0) {
						eos = 1;
					}
				}
			}

			os.clear();
			vb.clear();
			vd.clear();
			vi.clear();
		}

		oy.clear();

		try {
			if (bitStream != null) {
				bitStream.close();
			}
		} catch (Exception e) {
		}

		setStatus(Status.END_OF_SOUND);
	}

}
