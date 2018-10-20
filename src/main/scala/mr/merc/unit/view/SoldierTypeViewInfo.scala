package mr.merc.unit.view

import mr.merc.image.MImage
import scala.xml.XML
import scala.xml.Node
import scala.xml.NodeSeq
import mr.merc.map.hex._
import scalafx.scene.paint.Color
import mr.merc.unit.sound._
import mr.merc.sound.Sound
import mr.merc.sound.SoundConfig
import mr.merc.unit.SoldierType

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
    val xml = XML.load(getClass.getResourceAsStream("/conf/soldierTypesView.xml"))

    val parsed = xml \ "view" map (node => {
      val typeName = (node \ "@name").toString()
      val stand = parseImagesList(typeName, node \ "stand", Nil)
      val move = parseImagesList(typeName, node \ "move", stand)
      val idle = parseImagesList(typeName, node \ "idle", stand)
      val defence = parseImagesList(typeName, node \ "defence", stand)
      val death = parseImagesList(typeName, node \ "death", createDeathAnimation(stand.head))

      val images: Map[SoldierViewState, List[mr.merc.image.MImage]] = Map(DefenceState -> defence, IdleState -> idle, MoveState -> move,
        StandState -> stand, DeathState -> death, NoState -> List(MImage.emptyImage))
      val attacks = attacksMap(node, typeName)
      val attacksInfo = parseAttackViews(typeName, node)

      SoldierTypeViewInfo(typeName, images ++ attacks, parseSounds(typeName, node \ "sounds") mapValues SoundConfig.soundsMap, attacksInfo)
    })

    parsed.toList
  }

  private def parseAttackViews(name: String, node: NodeSeq): List[AttackView] = {
    var index = 0
    val list = collection.mutable.ArrayBuffer[AttackView]()
    while (getNode(node, "attack" + (index + 1)).nonEmpty) {
      val attackNode = getNode(node, "attack" + (index + 1)).get
      val imageName = (attackNode \ "@imageName").toString()
      val projectileName = (attackNode \ "@projectile").toString
      val attack = AttackView(index, imageName, if (projectileName == "") None else Some(projectileName))
      list += attack
      index += 1
    }

    list.toList
  }

  private def parseSounds(name: String, node: NodeSeq): Map[SoldierSound, String] = {
    val move = getNode(node, "move").map(_ \ "@sound").map(_.toString)
    val death = getNode(node, "death").map(_ \ "@sound").map(_.toString)
    val pain = getNode(node, "pain").map(_ \ "@sound").map(_.toString)

    val map = collection.mutable.Map[SoldierSound, String]()
    move foreach (m => map += (MovementSound -> m))
    death foreach (m => map += (DeathSound -> m))
    pain foreach (m => map += (PainSound -> m))

    for (number <- SoldierType(name).attacks.indices) {
      val attackNodeOpt = getNode(node, "attack" + (number + 1))
      attackNodeOpt match {
        case Some(attackNode) =>
          if ((attackNode \ "@succ").nonEmpty) {
            val succ = (attackNode \ "@succ").toString()
            map += AttackSound(number, success = true) -> succ
          }

          if ((attackNode \ "@fail").nonEmpty) {
            val fail = (attackNode \ "@fail").toString()
            map += AttackSound(number, success = false) -> fail
          }

        case None => // do nothing
      }
    }

    map.toMap
  }

  private def getNode(node: NodeSeq, name: String): Option[Node] = {
    (node \ name).headOption
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
    val absent = successes flatMap {case (s, list) =>
      val st = SoldierViewAttackState(success = false, s.direction, s.number)
      if (map(st).isEmpty) {
        Some(st, list)
      } else {
        None
      }
    }

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

    val fullSuccess = success.map(p => SoldierViewAttackState(success = true, p._1, attackNumber) -> p._2)
    val fullFail = fail.map(p => SoldierViewAttackState(success = false, p._1, attackNumber) -> p._2)

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
    val increment = BigDecimal(1.0) / size
    val result = (increment to 1.0 by increment).reverse
    val multiplyers = result.take(size)
    val alpha = (list zip multiplyers).map(p => p._1 * p._2)

    val imagesList = List.fill(size)(stand)
    (imagesList zip alpha).map{ case (image, a) => image.changeAlpha(a.toFloat)}
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
    val attr = node \ ("@" + attribute)
    if (attr.isEmpty) {
      default
    } else {
      attr.toString
    }
  }
}

case class SoldierTypeViewInfo(name: String, images: Map[SoldierViewState, List[MImage]], sounds: Map[SoldierSound, Sound], attacks: List[AttackView]) {
  def toColor(color: Color): SoldierTypeViewInfo = {
    val newImages = images.mapValues(_.map(_.changeSoldierColor(color))).toSeq.toMap
    SoldierTypeViewInfo(name, newImages, sounds, attacks)
  }

  def eagerLoad() {
    images.flatMap(_._2) foreach { im =>
      im.loadLazyImage()
      im.mirrorVertically.loadLazyImage()
    }
  }
}