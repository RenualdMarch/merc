package mr.merc.army

import mr.merc.army.WarriorCompetence.Professional
import mr.merc.economics.Culture.{CultureInfo, StateForm}
import mr.merc.economics.{Culture, FulfilledDemandRequest, PoliticalSystem, WarriorDemandRequest}
import mr.merc.economics.Population.Humans
import mr.merc.map.objects.House
import mr.merc.map.terrain.TerrainKind
import mr.merc.politics.{Party, State}
import mr.merc.unit.{Attack, AttackType, DefenceType, SoldierTypeAttribute}
import org.scalatest.{FunSuite, Matchers}
import scalafx.scene.paint.Color
import mr.merc.economics.MapUtil.FloatOperations._

class WarriorTest extends FunSuite with Matchers {

  val warriorType = new WarriorType("testWarriorType") {
    override def baseHp: Int = 100

    override def movement: Int = 5

    override def baseAttacks: List[Attack] = Nil

    override def moveCost: Map[TerrainKind, Int] = Map()

    override def defence: Map[DefenceType, Int] = Map()

    override def resistance: Map[AttackType, Int] = Map()

    override def attributes: Set[SoldierTypeAttribute] = Set()
  }

  val culture = new Culture("testCulture", Humans, "houseStyle", Color.White){
    override val warriorViewNames: WarriorViewNames = WarriorViewNames(Map((warriorType, Professional) -> "testType1"))
    override val cultureInfo: Culture.CultureInfo = CultureInfo(StateForm("a", "b"), Nil, Nil)
  }

  test("happy flow and healing") {
    val state = new State("testState", culture, 100000, new PoliticalSystem(Party.absolute))

    val w = new Warrior(warriorType, Professional, culture, state)
    w.hpPercentage shouldBe 1.0d +- 0.00001
    w.soldier.hp shouldBe 100
    w.hpPercentage = 0.8
    w.soldier.hp.toDouble shouldBe 80d +- 0.00001
    w.isAlive shouldBe true

    val sum = w.needs.values.sum

    val fullNeeds = w.needs.map { case (p, c) =>
      FulfilledDemandRequest(c, 1.0, WarriorDemandRequest(w, p, c))
    }

    fullNeeds.foreach(w.buyDemand)
    state.budget.moneyReserve shouldBe (100000 - sum) +- 0.00001
    w.allNeedsReceived(1)
    w.hpPercentage should be > 0.8
    w.historicalNeeds should have size 1
    val head = w.historicalNeeds.head
    head.demanded shouldBe head.received
    head.demanded shouldBe w.needs
  }

  test("damage") {
    val state = new State("testState", culture, 100000, new PoliticalSystem(Party.absolute))

    val w = new Warrior(warriorType, Professional, culture, state)

    val sum = w.needs.values.sum / 4

    val fullNeeds = w.needs.map { case (p, c) =>
      FulfilledDemandRequest(c / 4, 1.0, WarriorDemandRequest(w, p, c))
    }

    fullNeeds.foreach(w.buyDemand)
    state.budget.moneyReserve shouldBe (100000 - sum) +- 0.00001

    w.allNeedsReceived(1)
    w.hpPercentage should be < 1.0
    w.historicalNeeds should have size 1
    val head = w.historicalNeeds.head
    head.demanded shouldBe head.received |*| 4
    head.demanded shouldBe w.needs

    w.hpPercentage = 0
    w.soldier.hp shouldBe 0
    w.isAlive shouldBe false
  }
}
