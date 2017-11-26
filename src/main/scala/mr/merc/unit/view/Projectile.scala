package mr.merc.unit.view

import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.image.MImage
import mr.merc.map.hex.view.TerrainHexView
import scala.xml.XML
import java.io.File
import scala.xml.Node
import scala.xml.NodeSeq
import mr.merc.sound.Sound
import mr.merc.sound.SoundConfig

object Projectile {
  private val rootPath = "/images/projectiles/"

  private val map = parseProjectiles

  def apply(name: String) = map(name)

  private def parseProjectiles: Map[String, Projectile] = {
    val xml = XML.load(getClass.getResourceAsStream("/conf/projectiles.xml"))
    val parsed = (xml \ "projectile") map (node => {
      val name = (node \ "@name").toString()

      val move = parseState(name, node \ "move")
      val start = parseState(name, node \ "start")
      val end = parseState(name, node \ "end")

      val sounds = parseSounds(node \ "sounds") mapValues SoundConfig.soundsMap

      new Projectile(name, start, move, end, sounds)
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
    MImage(rootPath + projectileName.replace("-succ", "").replace("-fail", "") + "/" + name + ".png", x, y)
  }

  private def parseSounds(node: NodeSeq): Map[ProjectileSoundState, String] = {
    def getName(state: String): Option[String] = {
      val nodes = node \ state
      if (nodes.nonEmpty) {
        Some((nodes \ "@name") toString)
      } else {
        None
      }
    }

    val map = Map(ProjectileStartSound -> getName("start"),
      ProjectileMoveStartSound -> getName("move"),
      ProjectileEndSound -> getName("end"))

    map.filter { case (k, v) => v.isDefined } mapValues (_.get)
  }

  private def parseState(name: String, node: NodeSeq): Option[Map[Direction, List[MImage]]] = {
    if (node.isEmpty) {
      return None
    }

    val all = parseImagesList(name, node \ "all")
    val n = parseImagesList(name, node \ "n")
    val ne = parseImagesList(name, node \ "ne")

    if (all.nonEmpty) {
      Some(Direction.list map (_ -> all) toMap)
    } else {
      val nw = ne map (_.mirrorVertically)
      val sw = nw map (_.mirrorHorizontally)
      val s = n map (_.mirrorHorizontally)
      val se = ne map (_.mirrorHorizontally)
      Some(Map(N -> n, NE -> ne, NW -> nw, SW -> sw, S -> s, SE -> se))
    }
  }
}

class Projectile(val name: String, val start: Option[Map[Direction, List[MImage]]],
  val move: Option[Map[Direction, List[MImage]]], val end: Option[Map[Direction, List[MImage]]],
                 val sounds: Map[ProjectileSoundState, Sound]) {
  private val speed = 200

  def buildView(dir: Direction, from: (Int, Int), to: (Int, Int), factor: Double) =
    new ProjectileView(start.map(_(dir)), move.map(_(dir)), end.map(_(dir)),
      from, to, (speed * factor) toInt, factor, sounds)
}

sealed trait ProjectileSoundState
object ProjectileStartSound extends ProjectileSoundState
object ProjectileMoveStartSound extends ProjectileSoundState
object ProjectileEndSound extends ProjectileSoundState