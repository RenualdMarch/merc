package mr.merc.world

import java.time.LocalDate

class WorldCalendar(private var _today: LocalDate = LocalDate.now()) {
  def nextDay() {
    _today = today.plusDays(1)
  }

  def today = _today
}