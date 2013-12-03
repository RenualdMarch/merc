package mr.merc.unit.view

import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.image.MImage
import mr.merc.map.hex.view.TerrainHexView
import scala.xml.XML
import java.io.File
import scala.xml.Node
import scala.xml.NodeSeq

object Projectile {
  private val rootPath = "/images/projectiles/"

  private val map = parseProjectiles

  def apply(name: String) = map(name)

  private def parseProjectiles: Map[String, Projectile] = {
    val path = getClass.getResource("/conf/projectiles.xml").toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "projectile") map (node => {
      val name = (node \ "@name").toString()

      val move = parseState(name, node \ "move")
      val start = parseState(name, node \ "start")
      val end = parseState(name, node \ "end")

      new Projectile(name, start, move, end)
    })

    parsed map (p => (p.name, p)) toMap
  }

  private def firstIndicesOfLists(map: Map[Direction, List[MImage]]): Map[Direction, List[MImage]] = {
    map map (kv => (kv._1, List(kv._2.head)))
  }

  private def lastIndicesOfLists(map: Map[Direction, List[MImage]]): Map[Direction, List[MImage]] = {
    map map (kv => (kv._1, List(kv._2.last)))
  }

  private def parseImagesList(projectileName: String, node: NodeSeq): List[MImage] = {
    (node \ "image" map (parseImage(projectileName, _))).toList
  }

  private def parseImage(projectileName: String, node: Node): MImage = {
    val name = (node \ "@name").toString()
    val x = if ((node \ "@x").isEmpty) 0 else (node \ "@x").toString.toInt
    val y = if ((node \ "@y").isEmpty) 0 else (node \ "@y").toString.toInt
    MImage(rootPath + projectileName + "/" + name + ".png", x, y)
  }

  private def parseState(name: String, node: NodeSeq): Option[Map[Direction, List[MImage]]] = {
    if (node.isEmpty) {
      return None
    }

    val all = parseImagesList(name, node \ "all")
    val n = parseImagesList(name, node \ "n")
    val ne = parseImagesList(name, node \ "ne")

    if (!all.isEmpty) {
      Some(Direction.list map ((_ -> all)) toMap)
    } else {
      val nw = ne map (_.mirrorVertically)
      val sw = nw map (_.mirrorHorizontally)
      val s = n map (_.mirrorHorizontally)
      val se = ne map (_.mirrorHorizontally)
      Some(Map((N -> n), (NE -> ne), (NW -> nw), (SW -> sw), (S -> s), (SE -> se)))
    }
  }
}

class Projectile(val name: String, val start: Option[Map[Direction, List[MImage]]],
  val move: Option[Map[Direction, List[MImage]]],
  val end: Option[Map[Direction, List[MImage]]]) {
  private val speed = 100

  def buildView(dir: Direction, from: (Int, Int), to: (Int, Int)) =
    new ProjectileView(start.map(_(dir)), move.map(_(dir)), end.map(_(dir)),
      from, to, speed)
}