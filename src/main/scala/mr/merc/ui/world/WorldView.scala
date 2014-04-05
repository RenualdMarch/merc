package mr.merc.ui.world

import mr.merc.map.hex.view.TerrainHexFieldView
import mr.merc.map.world.Province
import mr.merc.ui.common.geom.PolygonSet
import mr.merc.map.world.WorldMap
import mr.merc.map.hex.view.TerrainHexView
import scalafx.scene.canvas.GraphicsContext
import scalafx.geometry.Rectangle2D
import mr.merc.map.view.SoldiersDrawer
import mr.merc.world.view.CharacterView
import mr.merc.map.view.MapView
import mr.merc.world.character.CharacterGenerator
import mr.merc.map.terrain.Mountain
import mr.merc.map.terrain.Water
import mr.merc.map.terrain.TerrainType
import scala.collection.mutable.ArrayBuffer
import mr.merc.world.character.HumanCharacter
import mr.merc.world.character.Character
import mr.merc.map.hex.TerrainHex
import mr.merc.log.Logging

class WorldView(worldMap: WorldMap) extends Logging {
  val soldierDrawer = new SoldiersDrawer[CharacterView]
  val hexFieldView = new TerrainHexFieldView(worldMap.hexField, soldierDrawer, Some(worldMap))

  private var characterHexes = Map[(Int, Int), CharacterView]()

  def characterOnHex(x: Int, y: Int) = characterHexes.get(x, y)

  def cityOnHex(x: Int, y: Int): Option[Province] = {
    val hex = worldMap.hexField.hex(x, y)
    val province = worldMap.provinceByHex(hex)
    val center = province.settlementHex
    if (center == hex) {
      Some(province)
    } else {
      None
    }
  }

  var humanCharacterPosition: Province = _
  val mapPositions: Map[Province, collection.mutable.Map[Option[Province], ArrayBuffer[(TerrainHexView, Option[CharacterView])]]] = {
    worldMap.countries.flatMap(_.provinces).map { p =>
      val mutableMap = collection.mutable.Map[Option[Province], ArrayBuffer[(TerrainHexView, Option[CharacterView])]]()
      val buffer = ArrayBuffer[(TerrainHexView, Option[CharacterView])]() ++ characterPlacesInProvinceCenter(p).map((_, None))
      mutableMap += (None -> buffer)

      worldMap.provinceConnections(p).foreach {
        case (n, i) =>
          val buffer = ArrayBuffer[(TerrainHexView, Option[CharacterView])]() ++ characterPlacesMovingToProvince(p, n).map((_, None))
          mutableMap += (Some(n) -> buffer)
      }

      (p -> mutableMap)
    } toMap
  }

  initCharacterViews()

  private def insertCharacterView(buffer: ArrayBuffer[(TerrainHexView, Option[CharacterView])], view: CharacterView, errorText: String): TerrainHexView = {
    val freeIndex = buffer.zipWithIndex.find(p => p._1._2.isEmpty).map(_._2).
      getOrElse(sys.error(errorText))
    val (hexView, _) = buffer(freeIndex)
    buffer.update(freeIndex, (hexView, Some(view)))
    characterHexes += ((hexView.hex.x, hexView.hex.y) -> view)
    hexView
  }

  def initCharacterViews() {
    worldMap.countries.flatMap(_.provinces).foreach { p =>
      p.characters.charactersInProvinceCenter.foreach { c =>
        val view = new CharacterView(c)
        c match {
          case human: HumanCharacter => {
            humanCharacterPosition = p
            view.coords = hexFieldView.hex(p.settlementHex.x, p.settlementHex.y).coords
            characterHexes += ((p.settlementHex.x, p.settlementHex.y) -> view)
          }
          case computer: Character => {
            val buffer = mapPositions(p)(None)
            val hexView = insertCharacterView(buffer, view, s"Not found free place in province ${p.settlement.nameKey}")
            view.coords = hexView.coords
          }
        }
        soldierDrawer.addSoldier(view)
      }
      p.characters.charactersInMovement.foreach {
        case (target, list) =>
          list foreach { c =>
            val view = new CharacterView(c)
            c match {
              case human: HumanCharacter => {
                humanCharacterPosition = p
                view.coords = hexFieldView.hex(p.settlementHex.x, p.settlementHex.y).coords
                characterHexes += ((p.settlementHex.x, p.settlementHex.y) -> view)
              }
              case computer: Character => {
                val buffer = mapPositions(p)(Some(target))
                val hexView = insertCharacterView(buffer, view, s"Not found free " +
                  "place in province ${p.settlement.nameKey} when moving to ${target.settlement.nameKey}")
                view.coords = hexView.coords
              }
            }
          }
      }
    }
  }

  def update(time: Int) {
    soldierDrawer.update(time)
  }

  def selectedArrow(x: Int, y: Int): Option[(TerrainHexView, TerrainHexView)] = {
    hexFieldView.worldMapArrows.find {
      case (s, f) =>
        val polygon = hexFieldView.arrowPolygon(s, f)
        polygon.isInside(x, y)
    }
  }

  def canvasLayers = hexFieldView.canvasWorldLayers

  private def arrowPolygon(start: Province, finish: Province): PolygonSet = {
    val startView = hexFieldView.hex(start.settlementHex.x, start.settlementHex.y)
    val finishView = hexFieldView.hex(finish.settlementHex.x, finish.settlementHex.y)
    hexFieldView.arrowPolygon(startView, finishView)
  }

  def validHexesTypes(hex: TerrainHex) = !Set[TerrainType](Water, Mountain).contains(hex.terrain)
  def characterPlacesInProvinceCenter(province: Province): List[TerrainHexView] = {
    val center = province.settlementHex
    val hexes = (province.hexes - center).toList.filter(validHexesTypes)

    hexes.sortBy(center.distance) map { h => hexFieldView.hex(h.x, h.y) }
  }

  def characterPlacesMovingToProvince(placement: Province, target: Province): List[TerrainHexView] = {
    val center = placement.settlementHex
    val targetCenter = target.settlementHex
    val hexes = (placement.hexes - center).toList.filter(validHexesTypes)
    hexes.sortBy(targetCenter.distance) map { h => hexFieldView.hex(h.x, h.y) }
  }

  def pixelHeight = hexFieldView.pixelHeight
  def pixelWidth = hexFieldView.pixelWidth

  def handleEvent(event: WorldViewEvent) {
    info(s"Event arrived: $event")
    event match {
      case ShowCityArrowsWorldViewEvent(list) => {
        val views = list.map {
          case (fromProvince, toProvince) =>
            val from = hexFieldView.hex(fromProvince.settlementHex)
            val to = hexFieldView.hex(toProvince.settlementHex)
            (from, to)
        }
        hexFieldView.worldMapArrows = views toSet
      }

      case DeselectCityArrow => hexFieldView.selectedWorldMapArrow = None
      case SelectCityArrow(from, to) => hexFieldView.selectedWorldMapArrow = Some(from, to)
    }
  }
}