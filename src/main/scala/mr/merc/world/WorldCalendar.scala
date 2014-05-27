package mr.merc.world

import java.time.LocalDateTime

class WorldCalendar(private var _today: LocalDateTime = LocalDateTime.now()) {
  def nextDay() {
    _today = today.plusDays(1)
  }

  def today = _today
}