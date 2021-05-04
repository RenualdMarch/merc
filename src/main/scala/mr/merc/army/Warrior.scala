package mr.merc.army

import mr.merc.economics.Culture
import mr.merc.economics.{FulfilledDemandRequest, Products, WarriorDemandRequest, WorldConstants}
import mr.merc.local.Localization
import mr.merc.players.Player
import mr.merc.politics.State
import mr.merc.unit.view.{SoldierView, StandState}
import mr.merc.unit.{Soldier, SoldierType}
import mr.merc.util.CacheFactoryMap
import mr.merc.economics.MapUtil.FloatOperations._
import WorldConstants.Army._
import mr.merc.army.Warrior.WarriorNeedRecord
import mr.merc.army.WarriorCompetence.{Militia, Professional, Ruler}
import scalafx.scene.image.Image

object Warrior {

  case class WarriorNeedRecord(demanded: Map[Products.Product, Double], received: Map[Products.Product, Double], turn: Int) {
    def needsReceivedPercentage: Double = received.values.sum / demanded.values.sum
  }

}

class Warrior(val warriorType: WarriorType, val competence: WarriorCompetence, val culture: Culture, val owner: State) {

  private var historicalNeedsRecords: Vector[WarriorNeedRecord] = Vector()
  private val historicalRecords = 30

  private var storedTechLevel = -1

  def historicalNeeds: Vector[WarriorNeedRecord] = historicalNeedsRecords

  private var currentNeeds: Map[Products.Product, Double] = Map()

  def hpPercentage: Double = soldier.hp.toDouble / soldierType.hp

  def hpPercentage_=(hp: Double): Unit = {
    soldier.hp = Math.round(soldierType.hp * hp).toInt
  }

  def warriorWeight: Double = competence match {
    case Militia => 1
    case Professional => 2
    case Ruler => 3
  }

  def isAlive: Boolean = soldier.hp > 0

  def typeName: String = Localization(warriorType.name)

  val player: Player = owner.toPlayer

  private def actualTechLevel: Int = owner.technologyLevel.technologyLevel

  def soldierType: SoldierType = warriorType.buildSoldierType(competence, culture, storedTechLevel)

  private var storedSoldier = soldier

  def soldier: Soldier = {
    if (actualTechLevel == storedTechLevel) {
      storedSoldier
    } else {
      storedTechLevel = actualTechLevel
      clearCache()
      storedSoldier = new Soldier(warriorType.name, soldierType, player)
      soldier
    }
  }

  private def clearCache(): Unit = {
    _cache = None
  }

  @transient private var _cache: Option[((Double, Boolean, Boolean)) => SoldierView] = None

  private def cache: ((Double, Boolean, Boolean)) => SoldierView = {
    if (actualTechLevel != storedTechLevel) {
      clearCache()
    }

    _cache match {
      case Some(value) => value
      case None =>
        _cache = Some(CacheFactoryMap.memo[(Double, Boolean, Boolean), SoldierView] {
          case (factor, circles, drawState) => new SoldierView(soldier, factor, circles, drawState)
        })
        _cache.get
    }
  }

  def soldierView(factor: Double, circles: Boolean, drawState: Boolean): SoldierView = {
    val sv = cache(factor, circles, drawState)
    sv.refreshBars()
    sv
  }

  def needs: Map[Products.Product, Double] = SoldierSupply(competence, actualTechLevel)

  def buyDemand(demand: FulfilledDemandRequest): Unit = {
    owner.budget.spendMoneyOnArmySupply(demand.spentMoney)
    currentNeeds |+|= demand.product -> demand.bought
    demand.currentSpentMoney += demand.spentMoney
  }

  def allNeedsReceived(turn: Int): Unit = {
    val expectedTotal = needs.values.sum
    val receivedTotal = currentNeeds.values.sum
    val newHp = NeedsToHP(receivedTotal / expectedTotal)
    if (newHp == 1d || newHp > hpPercentage) {
      hpPercentage += HpGainStep
    } else {
      hpPercentage -= HpLossStep
    }

    historicalNeedsRecords :+= WarriorNeedRecord(needs, currentNeeds, turn)
    currentNeeds = Map()
    historicalNeedsRecords = historicalNeeds.takeRight(historicalRecords)
  }

  def image(factor: Double): Image = soldierView(1, false, false).images(StandState).head.scaledImage(factor).image
}
