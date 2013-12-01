package mr.merc.sound

trait AudioEventsListener {
  def notifyStatusChange(newStatus: BaseAudioRenderer.Status)
}
