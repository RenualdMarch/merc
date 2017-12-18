package mr.merc.economics

import mr.merc.map.Grid

trait EconomicRegion {

  def economicNeighbours:Set[EconomicRegion]

}

class EconomicGrid(region:EconomicRegion) extends Grid[EconomicRegion] {

  // TODO add cases of war and economic blockades
  override def isBlocked(t: EconomicRegion) = false

  def neighbours(t: EconomicRegion): Set[EconomicRegion] = t.economicNeighbours

  // currently we think that movement from one province to another takes same time
  override def price(from: EconomicRegion, to: EconomicRegion) = 1
}
