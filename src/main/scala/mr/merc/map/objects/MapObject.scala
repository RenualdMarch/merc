package mr.merc.map.objects

import mr.merc.image.MImage
import mr.merc.map.hex.{Direction, TerrainHex, TerrainHexField}
import mr.merc.map.terrain.{GrassKind, HillKind, MountainKind, WaterKind}
import mr.merc.util.MercUtils


abstract class MapObject(val name:String) {
	def images(hex:TerrainHex, field:TerrainHexField):List[MImage]
}

abstract class OneImageMapObject(name: String) extends MapObject(name) {

  lazy val imagePaths:Vector[MImage] = {
    val result = Stream.from(1).map { i =>
      val path = s"/images/terrain/$name/$i.png"
      Option(getClass.getResource(path)).map(_ => path)
    }.takeWhile(_.nonEmpty).flatten.toVector

    require(result.nonEmpty, s"Failed to load images for map object $name")
    result.map(MImage.apply)
  }

  lazy val neighbours:Map[Direction, MImage] = {
    Direction.list.flatMap { d =>
      val path = s"/images/terrain/$name/${d.name.toString}.png"
      Option(getClass.getResource(path)).map(_ => d -> MImage(path))
    } toMap
  }

	def images(hex:TerrainHex, field:TerrainHexField):List[MImage] = {
    if (hex.mapObj.contains(this)) {
      val i = MercUtils.stablePseudoRandomIndex(hex.x, hex.y, imagePaths.size)
      List(imagePaths(i))
    } else {
      if (hex.terrain.is(GrassKind)) {
        field.neighboursWithDirections(hex).filter(_._2.mapObj.contains(this)).flatMap { case (dir, _) =>
          neighbours.get(dir)
        } toList
      } else {
        Nil
      }
    }
	}
}

