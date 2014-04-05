package mr.merc.ui.world

import scala.util.Either
import scala.util.Left
import scala.util.Right
import mr.merc.map.world.Settlement
import mr.merc.map.world.Province
import scalafx.geometry.Rectangle2D
import mr.merc.map.world.WorldMap
import mr.merc.world.character.Character
import scalafx.beans.property.ObjectProperty
import mr.merc.world.character.HumanCharacter

class WorldController {
  val worldMap = WorldMap.load("worldMap1")
  worldMap.initCharacters()
  worldMap.initHumanCharacter()
  val worldView = new WorldView(worldMap)
  var shownArrows: List[(Province, Province)] = Nil
  var selectedShownArrow: Option[Province] = None

  val selected: ObjectProperty[Option[Either[Character, Province]]] = new ObjectProperty()
  selected.value = None

  def update(time: Int) {

  }

  def onMouseLeftClick(x: Int, y: Int, viewRect: Rectangle2D) {
    val selectedCharacter = selectCharacter(x, y, viewRect)
    selectedCharacter match {
      case Some(c) => {
        this.selected.value = Some(Left(c))
        c match {
          case human: HumanCharacter => {
            // TODO remove this hack and save player position somewhere!
            val selectedProvince = selectCity(x, y, viewRect).get
            val neigs = worldMap.provinceConnections(selectedProvince)
            val pairs = neigs.map(n => (selectedProvince, n._1))
            shownArrows = pairs

            worldView.handleEvent(ShowCityArrowsWorldViewEvent(pairs))
          }
          case char: Character => {
            shownArrows = Nil
            worldView.handleEvent(ShowCityArrowsWorldViewEvent(Nil))
          }
        }
      }
      case None => {
        val selectedCity = selectCity(x, y, viewRect)
        selectedCity match {
          case Some(city) => this.selected.value = Some(Right(city))
          case None => this.selected.value = None
        }
      }
    }
  }

  def onMouseRightClick(x: Int, y: Int, viewRect: Rectangle2D) {

  }

  def onMouseMove(x: Int, y: Int, viewRect: Rectangle2D) {
    val arrow = worldView.selectedArrow(x + viewRect.minX.toInt, y + viewRect.minY.toInt)
    arrow match {
      case Some(selectedArrow) =>
        worldView.handleEvent(SelectCityArrow(selectedArrow._1, selectedArrow._2))
        selectedShownArrow = Some(worldMap.provinceByHex(selectedArrow._2.hex))
      case None =>
        if (selectedShownArrow.nonEmpty) {
          selectedShownArrow = None
          worldView.handleEvent(DeselectCityArrow)
        }
    }
  }

  def selectCharacter(x: Int, y: Int, viewRect: Rectangle2D): Option[Character] = {
    val hexView = worldView.hexFieldView.hexByPixelCoords(x + viewRect.minX.toInt, y + viewRect.minY.toInt)
    hexView.flatMap { h =>
      val x = h.hex.x
      val y = h.hex.y
      worldView.characterOnHex(x, y).map(_.character)
    }
  }

  def selectCity(x: Int, y: Int, viewRect: Rectangle2D): Option[Province] = {
    val hexView = worldView.hexFieldView.hexByPixelCoords(x + viewRect.minX.toInt, y + viewRect.minY.toInt)
    hexView flatMap { h =>
      val x = h.hex.x
      val y = h.hex.y
      worldView.cityOnHex(x, y)
    }
  }
}