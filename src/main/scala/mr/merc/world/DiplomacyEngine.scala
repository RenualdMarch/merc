package mr.merc.world

class DiplomacyEngine {
  var countries: Set[Country] = Set()
  var currentWars: List[War] = Nil
  def areEnemies(first: Country, second: Country): Boolean = {
    currentWars.exists(war => Set(war.firstSide, war.secondSide) == Set(first, second))
  }
}