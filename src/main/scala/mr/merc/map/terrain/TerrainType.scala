package mr.merc.map.terrain

import mr.merc.image.MImage
import mr.merc.util.MercUtils

object TerrainType {
  def list:List[TerrainType] = List(GreenGrass, DryGrass, SemidryGrass, ShallowWater, BasicMountain, BasicMountainSnow, DesertSand,
    BasicHill, BasicHillSnow, CleanRoad, OldRoad, DirtRoad, GrassyRoad, DecForest, DecForestFall, DecForestWinter,
    PineForest, MixedForest, Castle, Mud, LeafLitter, Snow, Ice)

  def helperTypesList:List[TerrainType] = List(BankInside, BankOutside)
}

abstract sealed class TerrainType(val name: String, val kind:TerrainKind, val belowTerrainType:Option[TerrainType] = None) {
  lazy val imagePaths:Vector[MImage] = {
    val result = Stream.from(1).map { i =>
      val path = s"/images/terrain/$name/$i.png"
      Option(getClass.getResource(path)).map(_ => path)
    }.takeWhile(_.nonEmpty).flatten.toVector

    require(result.nonEmpty, s"Failed to load images for map object $name")
    result.map(MImage.apply)
  }

  def image(x: Int, y: Int):MImage = {
    val i = MercUtils.stablePseudoRandomIndex(x, y, imagePaths.size)
    imagePaths(i)
  }

  def is(kind:TerrainKind):Boolean = kind == this.kind

  def isNot(kind:TerrainKind):Boolean = !is(kind)

  def isOneOf(kinds:TerrainKind*):Boolean = kinds.exists(is)

  def isNotOneOf(kinds:TerrainKind*):Boolean = !isOneOf(kinds:_*)
}

sealed abstract class TerrainKind()

case object GrassKind extends TerrainKind
case object WaterKind extends TerrainKind
case object MountainKind extends TerrainKind
case object SandKind extends TerrainKind
case object HillKind extends TerrainKind
case object RoadKind extends TerrainKind
case object ForestKind extends TerrainKind
case object WallsKind extends TerrainKind
case object SwampKind extends TerrainKind
case object SnowKind extends TerrainKind
case object IceKind extends TerrainKind
case object EmptyKind extends TerrainKind

case object GreenGrass extends TerrainType("green", GrassKind)
case object DryGrass extends TerrainType("dry", GrassKind)
case object SemidryGrass extends TerrainType("semidry", GrassKind)
case object LeafLitter extends TerrainType(name="leafLitter", GrassKind)
case object Farm extends TerrainType("farm", GrassKind, belowTerrainType = Some(LeafLitter))

case object ShallowWater extends TerrainType("water", WaterKind)

case object BasicMountain extends TerrainType("mountain", MountainKind,belowTerrainType = Some(BasicHill))
case object BasicMountainSnow extends TerrainType("mountainSnow", MountainKind,belowTerrainType = Some(BasicHillSnow))


case object DesertSand extends TerrainType("sand", SandKind)
case object BasicHill extends TerrainType("hill", HillKind)
case object BasicHillSnow extends TerrainType("hillSnow", HillKind)

case object CleanRoad extends TerrainType("cleanRoad", RoadKind)
case object OldRoad extends TerrainType("oldRoad", RoadKind)
case object DirtRoad extends TerrainType("dirt", RoadKind)
case object GrassyRoad extends TerrainType("grassyRoad", RoadKind)

case object DecForest extends TerrainType("decForest", ForestKind, belowTerrainType = Some(GreenGrass))
case object DecForestFall extends TerrainType("decForestFall", ForestKind, belowTerrainType = Some(DryGrass))
case object DecForestWinter extends TerrainType("decForestWinter", ForestKind, belowTerrainType = Some(Snow))


case object PineForest extends TerrainType("pineForest", ForestKind, belowTerrainType = Some(GreenGrass))
case object MixedForest extends TerrainType("mixedForest", ForestKind, belowTerrainType = Some(GreenGrass))

case object Snow extends TerrainType("snow", SnowKind)

case object Ice extends TerrainType("ice", IceKind)

case object Castle extends TerrainType("cobbles", WallsKind) {
  override lazy val imagePaths: Vector[MImage] = {
    Vector("/images/terrain/walls/cobbles.png").map(MImage.apply)
  }
}

// helper types
case object BankInside extends TerrainType("bankInside", WaterKind)
case object BankOutside extends TerrainType("bankOutside", WaterKind)

// TODO work on it
case object Mud extends TerrainType("swamp", SwampKind)
// THIS TYPES ARE FORBIDDEN TO USE ON MAP
case object Empty extends TerrainType("void", EmptyKind) {
}
