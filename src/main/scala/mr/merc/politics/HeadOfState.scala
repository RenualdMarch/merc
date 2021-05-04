package mr.merc.politics

import mr.merc.army.{WarriorCompetence, WarriorType}
import mr.merc.economics.{Culture, SeasonOfYear}
import mr.merc.local.Localization

object HeadOfState {
  case class RulingTime(from: SeasonOfYear, to:SeasonOfYear, head: HeadOfState)
}

sealed abstract class HeadOfState(name: String, state: State, wt: WarriorType, wc: WarriorCompetence,
                  born: SeasonOfYear, projectedDeath: SeasonOfYear)
  extends Person(name, state, state.primeCulture, wt, wc, born, projectedDeath) {
}

class Monarch (name: String, number: Int, state: State, wt: WarriorType, wc: WarriorCompetence,
  born: SeasonOfYear, projectedDeath: SeasonOfYear) extends HeadOfState(name, state, wt, wc, born, projectedDeath) {

  import mr.merc.util.MercUtils._
  override def fullName: String = s"${Localization(state.primeCulture.cultureInfo.stateForm.monarchyTitle)} $name ${number.toRomanString}"
}

class President(name: String, state: State, wt: WarriorType, wc: WarriorCompetence,
                  born: SeasonOfYear, projectedDeath: SeasonOfYear)
  extends HeadOfState(name, state, wt, wc, born, projectedDeath) {

  override def fullName: String = s"${Localization(state.primeCulture.cultureInfo.stateForm.democracyTitle)} $name"

  def toMonarch(number:Int): Monarch = new Monarch(name, number, state, wt, wc, born, projectedDeath)
}

class Official(name: String, val jobTitle:String, state: State, culture:Culture, wt: WarriorType, wc: WarriorCompetence,
               born: SeasonOfYear, projectedDeath: SeasonOfYear)
  extends Person(name, state, culture, wt, wc, born, projectedDeath) {

  override def fullName: String = s"$jobTitle $name"
}