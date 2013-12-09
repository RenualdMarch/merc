package mr.merc.unit.view

import mr.merc.image.MImage
import scala.xml.XML
import java.io.File
import scala.xml.Node
import scala.xml.NodeSeq
import mr.merc.map.hex._
import scalafx.scene.paint.Color
import mr.merc.unit.Attack

object SoldierTypeViewInfo {
  val rootPath = "/images/units/"
  private val colorsCache = collection.mutable.Map[(String, Color), SoldierTypeViewInfo]()

  private val rawTypes = parse.map(p => (p.name, p)).toMap

  def apply(name: String) = rawTypes(name)

  def apply(name: String, color: Color): SoldierTypeViewInfo = {
    if (colorsCache.contains(name, color)) {
      colorsCache(name, color)
    } else {
      val result = apply(name).toColor(color)
      colorsCache += ((name, color) -> result)
      result
    }

  }

  private def parse: List[SoldierTypeViewInfo] = {
    val path = getClass.getResource("/conf/soldierTypesView.xml").toURI
    val xml = XML.loadFile(new File(path))

    val parsed = xml \ "view" map (node => {
      val typeName = (node \ "@name").toString()
      val stand = parseImagesList(typeName, node \ "stand", Nil)
      val move = parseImagesList(typeName, node \ "move", stand)
      val idle = parseImagesList(typeName, node \ "idle", stand)
      val defence = parseImagesList(typeName, node \ "defence", stand)
      val death = parseImagesList(typeName, node \ "death", createDeathAnimation(stand(0)))

      val images: Map[SoldierViewState, List[mr.merc.image.MImage]] = Map(DefenceState -> defence, IdleState -> idle, MoveState -> move,
        StandState -> stand, DeathState -> death, NoState -> List(MImage.emptyImage))
      val attacks = attacksMap(node, typeName)

      SoldierTypeViewInfo(typeName, images ++ attacks)
    })

    parsed.toList
  }

  private def attacksMap(typeNode: Node, typeName: String): Map[SoldierViewAttackState, List[MImage]] = {
    var attacks = Vector[Map[SoldierViewAttackState, List[MImage]]]()
    var number = 0
    while (containsAttack(typeNode, number)) {
      val attack = parseAttack(typeNode, typeName, number)
      attacks :+= attack
      number += 1
    }

    val withoutAbsentFails = replaceAbsentFailWithSuccess(attacks.foldLeft(Map[SoldierViewAttackState, List[MImage]]())(_ ++ _))
    val withoutAbsentTypes = addSWandNW(withoutAbsentFails)
    withoutAbsentTypes
  }

  private def replaceAbsentFailWithSuccess(map: Map[SoldierViewAttackState, List[MImage]]): Map[SoldierViewAttackState, List[MImage]] = {
    val successes = map.filter(_._1.success)
    val absent = successes flatMap (s => {
      val st = SoldierViewAttackState(false, s._1.direction, s._1.number)
      if (map(st).isEmpty) {
        Some(st, s._2)
      } else {
        None
      }
    })

    map ++ absent
  }

  private def replaceAbsentAttack2WithAttack1(map: Map[SoldierViewAttackState, List[MImage]]): Map[SoldierViewAttackState, List[MImage]] = {
    val attack1 = map.filter(_._1.number == 0)
    val absent = attack1 flatMap (a => {
      val st = SoldierViewAttackState(a._1.success, a._1.direction, 1)
      if (map(st).isEmpty) {
        Some(st, a._2)
      } else {
        None
      }
    })

    map ++ absent
  }

  private def containsAttack(typeNode: Node, attackNumber: Int): Boolean = {
    val attackName = "attack" + (attackNumber + 1)
    (typeNode \ attackName).nonEmpty
  }

  private def parseAttack(typeNode: Node, typeName: String, attackNumber: Int): Map[SoldierViewAttackState, List[MImage]] = {
    val attackName = "attack" + (attackNumber + 1)
    val directions = List(N, NE, SE, S)
    val success = directions map (d => (d, parseAttackDirection(typeNode, typeName, attackName, "succ", d))) toMap
    val fail = directions map (d => (d, parseAttackDirection(typeNode, typeName, attackName, "fail", d))) toMap

    val fullSuccess = success.map(p => (SoldierViewAttackState(true, p._1, attackNumber) -> p._2))
    val fullFail = fail.map(p => (SoldierViewAttackState(false, p._1, attackNumber) -> p._2))

    fullFail ++ fullSuccess
  }

  private def addSWandNW(map: Map[SoldierViewAttackState, List[MImage]]): Map[SoldierViewAttackState, List[MImage]] = {
    val added = map flatMap (p => {
      if (p._1.direction == SE) {
        val st = SoldierViewAttackState(p._1.success, SW, p._1.number)
        Some((st, p._2))
      } else if (p._1.direction == NE) {
        val st = SoldierViewAttackState(p._1.success, NW, p._1.number)
        Some((st, p._2))
      } else {
        None
      }
    })

    map ++ added
  }

  private def parseAttackDirection(typeNode: Node, typeName: String, attackName: String, success: String, direction: Direction): List[MImage] = {
    val images = parseImagesList(typeName, typeNode \ attackName \ direction.toString().toLowerCase() \ success, Nil)
    val allImages = parseImagesList(typeName, typeNode \ attackName \ "all" \ success, Nil)

    if (images.isEmpty) {
      allImages
    } else {
      images
    }
  }

  private def createDeathAnimation(stand: MImage): List[MImage] = {
    val size = 5
    val startingAlpha = stand.alpha
    val list = List.fill(size)(startingAlpha)
    val increment = 1.0f / size
    val result = increment.to(1.0f).by(increment).reverse
    val multiplyers = result.take(size)
    val alpha = (list zip multiplyers).map(p => p._1 * p._2)

    val imagesList = List.fill(size)(stand)
    (imagesList zip alpha).map(p => p._1.changeAlpha(p._2))
  }

  private def parseImagesList(typeName: String, node: NodeSeq, default: List[MImage]): List[MImage] = {
    if ((node \ "image").isEmpty) {
      default
    } else {
      (node \ "image" map (parseImage(typeName, _))).toList
    }
  }

  private def parseImage(typeName: String, node: Node): MImage = {
    val name = (node \ "@name").toString()
    val alpha = getOrElse(node, "alpha", "1").toFloat
    val x = getOrElse(node, "x", "0").toInt
    val y = getOrElse(node, "y", "0").toInt

    MImage(rootPath + typeName + "/" + name + ".png", x, y, alpha)
  }

  private def getOrElse(node: Node, attribute: String, default: String): String = {
    val attr = (node \ ("@" + attribute))
    if (attr.isEmpty) {
      default
    } else {
      attr.toString
    }
  }
}

case class SoldierTypeViewInfo(name: String, images: Map[SoldierViewState, List[MImage]]) {
  def toColor(color: Color): SoldierTypeViewInfo = {
    val newImages = images.mapValues(_.map(_.changeSoldierColor(color))).toSeq.toMap
    SoldierTypeViewInfo(name, newImages)
  }
}