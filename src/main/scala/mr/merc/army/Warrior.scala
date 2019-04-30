package mr.merc.army

import mr.merc.ai.BattleAI
import mr.merc.army.WarriorType.WarriorCompetence
import mr.merc.economics.Population.Culture
import mr.merc.local.Localization
import mr.merc.players.Player
import mr.merc.politics.State
import mr.merc.unit.view.SoldierView
import mr.merc.unit.{Soldier, SoldierType}
import mr.merc.util.CacheFactoryMap

class Warrior(warriorType: WarriorType, competence: WarriorCompetence, culture: Culture, owner: State) {

  def hpPercentage:Double = soldier.hp.toDouble / soldierType.hp

  def hpPercentage_=(hp: Double): Unit = {
    soldier.hp = Math.round(soldierType.hp * hp).toInt
  }

  def isAlive:Boolean = soldier.hp > 0

  def typeName:String = Localization(warriorType.name)

  val player:Player = Player(owner.name, owner.color, None)

  def soldierType:SoldierType = warriorType.buildSoldierType(competence, culture)

  val soldier:Soldier = new Soldier(warriorType.name, soldierType, player)

  private val cache = CacheFactoryMap.memo[(Double, Boolean), SoldierView] {
    case (factor, circles) => new SoldierView(soldier, factor, circles, circles)
  }

  def soldierView(factor: Double, circles:Boolean):SoldierView = {
    val sv = cache(factor, circles)
    sv.refreshBars()
    sv
  }

}
