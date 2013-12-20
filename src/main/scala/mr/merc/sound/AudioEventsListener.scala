package mr.merc.sound

// TODO make into function
trait AudioEventsListener {
  def apply(newStatus: BaseAudioRenderer.Status)
}
