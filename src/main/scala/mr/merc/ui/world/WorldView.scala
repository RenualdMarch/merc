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

class WorldView(worldMap: WorldMap) {
  val soldierDrawer = new SoldiersDrawer[CharacterView]
  val hexFieldView = new TerrainHexFieldView(worldMap.hexField, soldierDrawer, Some(worldMap))

  var humanCharacterPosition: Province = _
  val mapPositions: Map[Province, collection.mutable.Map[Option[Province], ArrayBuffer[(TerrainHexView, Option[CharacterView])]]] = {
    worldMap.countries.flatMap(_.provinces).map { p =>
      val mutableMap = collection.mutable.Map[Option[Province], ArrayBuffer[(TerrainHexView, Option[CharacterView])]]()
      val buffer = ArrayBuffer[(TerrainHexView, Option[CharacterView])]() ++ characterPlacesInProvinceCenter(p).map((_, None))
      mutableMap += (None -> buffer)
      // TODO add to province movements
      (p -> mutableMap)
    } toMap
  }

  initCharacterViews()

  private def insertCharacterView(buffer: ArrayBuffer[(TerrainHexView, Option[CharacterView])], view: CharacterView, errorText: String): TerrainHexView = {
    val freeIndex = buffer.zipWithIndex.find(p => p._1._2.isEmpty).map(_._2).
      getOrElse(sys.error(errorText))
    val (hexView, _) = buffer(freeIndex)
    buffer.update(freeIndex, (hexView, Some(view)))
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
          }
          case computer: Character => {
            val buffer = mapPositions(p)(None)
            val hexView = insertCharacterView(buffer, view, s"Not found free place in province ${p.settlement.nameKey}")
            view.coords = hexView.coords
          }
        }
        soldierDrawer.addSoldier(view)

        // TODO insert character in province borders
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

  def characterPlacesInProvinceCenter(province: Province): List[TerrainHexView] = {
    val invalidHexesTypes = Set[TerrainType](Water, Mountain)
    val center = province.settlementHex
    val hexes = (province.hexes - center).toList.filter(h => !invalidHexesTypes.contains(h.terrain))

    hexes.sortBy(center.distance) map { h => hexFieldView.hex(h.x, h.y) }
  }

  def pixelHeight = hexFieldView.pixelHeight
  def pixelWidth = hexFieldView.pixelWidth
}