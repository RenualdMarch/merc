package mr.merc.diplomacy

import mr.merc.economics.Culture.{FrenchHuman, GermanHuman, LatinHuman}
import mr.merc.economics._
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.GreenGrass
import mr.merc.players.{ColorGenerator, NamesGenerator}
import mr.merc.politics.{Party, Province, State}
import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}
import scalafx.scene.paint.Color

class AbstractDiplomacyTest extends FunSuite with Matchers with BeforeAndAfter {

  val colorGenerator = new ColorGenerator()

  var actions: WorldStateDiplomacyActions = _

  def states:List[State] = actions.regions.map(_.owner).distinct

  before {
    actions = new WorldStateDiplomacyActions {

      override val regions: List[Province] = generateRegions()

      override def namesGenerators: Map[Culture, NamesGenerator] = cultures.map { c =>
        c -> new NamesGenerator(c.cultureInfo)
      } toMap

      override def turn: Int = 1

      override var colorStream: Stream[Color] = ColorGenerator.colorStream
    }
  }

  after {
    actions = null
  }


  def generateStates(): Set[State] = {
    (0 until statesCount).zip(cultures).map { case (n, c) =>
      new State(n.toString, c, 1000, new PoliticalSystem(Party.absolute))
    }.toSet
  }

  def cultures = List(FrenchHuman, GermanHuman, LatinHuman)

  def statesCount = 3

  def provincePerState = 2

  def regionPopulationFor(s: State, n: Number): RegionPopulation = new RegionPopulation(Nil)

  def generateRegions(): List[Province] = {
    val allRegions = generateStates().toList.flatMap { s =>
      (0 until provincePerState).map { i =>
        val hex = new TerrainHex(0, 0, GreenGrass)
        new Province(i.toString, s, new RegionMarket(Map()), regionPopulationFor(s, i), Set(hex), hex)
      }
    }

    allRegions.foreach { r =>
      r.initNeighbours(allRegions.toSet - r)
    }

    allRegions
  }

  def sendAndAccept(message:DiplomaticProposal): Unit = {
    actions.sendMessage(message)
    actions.answerMessage(message, true)
  }

  def sendAndDecline(message:DiplomaticProposal): Unit = {
    actions.sendMessage(message)
    actions.answerMessage(message, false)
  }
}
