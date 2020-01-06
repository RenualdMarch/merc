package mr.merc.map.view

import mr.merc.map.hex.view.{TerrainHexFieldView, TerrainHexView}
import mr.merc.map.hex.TerrainHexField
import mr.merc.unit.view.SoldierView
import mr.merc.view.move.Movement
import mr.merc.log.Logging
import mr.merc.map.hex.view.TerrainHexFieldView.{BattleFieldViewMode, FieldViewMode}
import mr.merc.map.terrain.WaterKind
import mr.merc.politics.Province

// TODO add update method which handles case when soldiers changed
class MapView(field: TerrainHexField, factor: Double, val soldiersDrawer: SoldiersDrawer[SoldierView] = new SoldiersDrawer[SoldierView](), mode:FieldViewMode = BattleFieldViewMode) extends Logging {
  val terrainView = new TerrainHexFieldView(field, soldiersDrawer, factor, mode)

  if (mode == BattleFieldViewMode) {
    createSoldiers foreach (soldiersDrawer.addSoldier)
  }


  def hexByPixel(x: Int, y: Int):Option[TerrainHexView] = terrainView.hexByPixelCoords(x, y)

  def provinceByPixel(x: Int, y: Int):Option[Province] = {
    val hexOpt = hexByPixel(x, y)
    hexOpt.filter(_.hex.terrain.isNot(WaterKind)).flatMap(_.hex.province)
  }

  def soldiers = soldiersDrawer.soldiers

  private def createSoldiers: List[SoldierView] = {
    val soldiers = terrainView.realHexes.flatMap(h => {
      val soldierOption = h.hex.soldier
      soldierOption match {
        case Some(soldier) =>
          debug(s"Creating soldier view for soldier type ${soldier.soldierType.name}")
          val view = new SoldierView(soldier, factor)
          view.coords = (h.x, h.y)
          Some(view)
        case None => None
      }

    })

    soldiers toList
  }

  def update(time: Int) {
    soldiersDrawer.update(time)
  }

  def canvasBattleLayers = terrainView.canvasLayers

  def addMovement(movement: Movement) {
    soldiersDrawer.addMovement(movement)
  }

  def pixelWidth = terrainView.pixelWidth
  def pixelHeight = terrainView.pixelHeight
}