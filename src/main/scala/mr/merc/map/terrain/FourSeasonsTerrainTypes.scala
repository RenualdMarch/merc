package mr.merc.map.terrain

import mr.merc.economics.{Culture, FourSeasonsMapObject}
import mr.merc.economics.Seasons.{Autumn, Spring, Summer, Winter}
import mr.merc.map.objects.{Flowers, Signpost, SummerHouse, WinterHouse, WoodenBridge}

object FourSeasonsTerrainTypes {
  import mr.merc.economics.Seasons._

  type FourSeasonsTerrainType = Map[Season, TerrainType]

  val FourSeasonsGrass = Map(Spring -> GreenGrass, Summer -> SemidryGrass, Autumn -> DryGrass, Winter -> Snow)
  val FourSeasonsDecForest = Map(Spring -> DecForest, Summer -> DecForest, Autumn -> DecForestFall, Winter -> DecForestWinter)
  val FourSeasonsHill = Map(Spring -> BasicHill, Summer -> BasicHill, Autumn -> BasicHill, Winter -> BasicHillSnow)
  val FourSeasonsMountain = Map(Spring -> BasicMountain, Summer -> BasicMountain, Autumn -> BasicMountain, Winter -> BasicMountainSnow)
  val FourSeasonsWater = Map(Spring -> ShallowWater, Summer -> ShallowWater, Autumn -> ShallowWater, Winter -> ShallowWater)
  val FourSeasonsRiver = Map(Spring -> ShallowWater, Summer -> ShallowWater, Autumn -> ShallowWater, Winter -> Ice)
  val FourSeasonsRoad = Map(Spring -> GrassyRoad, Summer -> GrassyRoad, Autumn -> GrassyRoad, Winter -> OldRoad)
  val FourSeasonsCastle = Map(Spring -> Castle, Summer -> Castle, Autumn -> Castle, Winter -> Castle)
}

object FourSeasonsMapObjects {
  val FourSeasonsWoodenBridge = new FourSeasonsMapObject(Map(Spring -> WoodenBridge, Summer -> WoodenBridge, Autumn -> WoodenBridge, Winter -> WoodenBridge))

  case class FourSeasonsHouse(culture: Culture) extends FourSeasonsMapObject(
    Map(Spring -> SummerHouse(culture), Summer -> SummerHouse(culture), Autumn -> SummerHouse(culture), Winter -> WinterHouse(culture)))

  val FourSeasonsFlowers = new FourSeasonsMapObject(Map(Spring -> Flowers, Summer -> Flowers, Autumn -> Flowers))
  val FourSeasonsSignpost = new FourSeasonsMapObject(Map(Spring -> Signpost, Summer -> Signpost, Autumn -> Signpost, Winter -> Signpost))
}