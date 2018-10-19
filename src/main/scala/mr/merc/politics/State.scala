package mr.merc.politics

import mr.merc.economics.Population.Culture
import mr.merc.economics._
import mr.merc.map.hex.TerrainHex

import scalafx.scene.paint.Color

case class State(name: String, primeCulture:Culture, budget: StateBudget, taxPolicy: TaxPolicy,color: Color = Color.Black) {

}

class Province(var owner: State, val regionMarket: RegionMarket,
               val regionPopulation: RegionPopulation, hexes: Set[TerrainHex]) extends EconomicRegion {

  private var neighbourProvinces:Option[Set[Province]] = None

  def initNeighbours(set:Set[Province]): Unit = {
    this.neighbourProvinces = Some(set)
  }

  override def economicNeighbours: Set[EconomicRegion] = {
    neighbourProvinces.getOrElse(Set()).asInstanceOf[Set[EconomicRegion]]
  }
}