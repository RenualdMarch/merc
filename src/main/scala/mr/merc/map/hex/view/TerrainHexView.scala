package mr.merc.map.hex.view

import scalafx.scene.canvas.GraphicsContext
import mr.merc.image.MImage
import mr.merc.map.terrain.Grass
import mr.merc.map.terrain.Forest
import mr.merc.map.hex.TerrainHex
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.CubeHex
import scalafx.scene.paint.Color
import mr.merc.map.hex.Direction
import mr.merc.image.MImageCache

object TerrainHexView {
  val Side = 72
}

class TerrainHexView(val hex:TerrainHex, field:TerrainHexField) {
    private val arrowPath = "/images/arrows/"
  
    val neighbours = field.neighboursWithDirections(hex.x, hex.y)
    val directions = neighbours map (p => (p._2, p._1)) toMap
	val side = TerrainHexView.Side
    val x = findX
	val y = findY
	
	override def toString = s"TerrainHexView[coords=($hex.x, $hex.y), pixels=($x,$y)]" 
	
	private def findX = if (hex.x % 2 == 0) {
	  hex.x * 6 * side / 8
	} else {
	  (hex.x - 1) * 6 * side / 8 + side * 3 / 4 
	}
	
	private def findY = if (hex.x % 2 == 0) {
	  side * hex.y
	} else {
	  side * hex.y + side / 2
	}
	
	private lazy val elements:List[TerrainHexViewAdditiveElement] = {
	  val additives = TerrainHexViewAdditive.extractAdditives(this)
	  val rule = new TerrainHexViewAdditiveRule
	  rule.transform(additives)
	}
	
	val image:MImage = {
	  if (hex.terrain == Forest) {
	    MImage(Grass.imagePath)
	  } else {
	    MImage(hex.terrain.imagePath)
	  }	  
	}
	  
	val secondaryImage:Option[MImage] = {
	  if (hex.terrain == Forest) {
	    Some(MImage(Forest.imagePath))
	  } else {
	    None
	  }
	}
	
	val mapObject = hex.mapObj match {
	    case Some(mapObj) => mapObj.images(hex, field)
	    case None => Nil
	}
	
	val neighbourMapObjects:List[MImage] = {
	  val neigMapObj = field.neighbours(hex).filter(p => p.mapObj != None && p.mapObj != hex.mapObj)
	  neigMapObj.flatMap(p => p.mapObj.get.images(hex, field)).toList
	}
	
	def drawItself(gc:GraphicsContext) {
	  image.drawCenteredImage(gc, x, y, side, side)
	  elements foreach (_.drawItself(gc, x, y))
	  neighbourMapObjects foreach (_.drawCenteredImage(gc, x, y, side, side))
	  secondaryImage.map(_.drawCenteredImage(gc, x, y, side, side))
	  mapObject foreach (_.drawCenteredImage(gc, x, y, side, side))	
	}
	
	def drawMovementImpossible(gc:GraphicsContext) {
	  gc.save()
	  gc.globalAlpha = 0.3
	  gc.fill = Color.BLACK
	  gc.fillPolygon(angles)
	  gc.restore()
	}
	
	def drawArrowStart(gc:GraphicsContext, dir:Direction) {
	  val path = arrowPath + "attack-indicator-src-" + dir.toString.toLowerCase() + ".png"
	  MImage(path).drawImage(gc, x, y)
	}
	
	def drawArrowEnd(gc:GraphicsContext, dir:Direction) {
	  val path = arrowPath + "attack-indicator-dst-" + dir.toString.toLowerCase() + ".png"
	  MImage(path).drawImage(gc, x, y)
	}
	
	def center = (x + side / 2, y + side / 2)
	def coords = (x, y)
	def angles:Seq[(Double, Double)] = {
	  val a = side / 4 toDouble
	  val coef = Seq((1, 0), (3, 0), (4, 2), (3, 4), (1, 4), (0, 2))
	  coef.map { case (x1, y1) => (x + x1 * a, y + y1 * a)}
	} 
}