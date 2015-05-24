package mr.merc.person

import java.time.LocalDate

case class Person(name: PersonName, born:LocalDate, psycho: Psycho, exp: Exp, nationality: Nationality) {

}

case class PersonName(name: String, surname: String, nick: String)

class Exp(talent: Double, level: Int) {
  private var expPoints: Int = 0


}

