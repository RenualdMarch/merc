package mr.merc.map.hex

import scala.math._
import mr.merc.map.Grid

class HexField(val width:Int, val height:Int) extends Grid[Hex] {
	private val arr = Array.ofDim[Hex](width, height)
	for (x <- 0 until width; y <- 0 until height) {
	  if (isLegalCoords(x, y)) { 
		  arr(x)(y) = new Hex(x, y)
	  }
	}
	
	def isLegalCoords(x:Int, y:Int):Boolean = {
	  if (y % 2 == 1 && x == width - 1) {
	    return false
	  }
	  
	  if (x >= width || y >= height || x < 0 || y < 0) {
	    false
	  } else {
	    true
	  }
	}
	
	def hex(x:Int, y:Int) = {
	  require(isLegalCoords(x, y), s"x=$x and y=$y are illegal coords!")
	  arr(x)(y)
	}
	
	def neighbours(hex:Hex):Set[Hex] = neighbours(hex.x, hex.y)
	
	def neighbours(x:Int, y:Int):Set[Hex] = {
		// y % 2 == 1 is even because we start from zero
		val corrections = correctionsSet(y % 2 == 1)
		val allNeighboursCoords = corrections.map(h => (h._1 + x, h._2 + y))
		val correctCoords = allNeighboursCoords.filter(h => isLegalCoords(h._1, h._2))
		correctCoords.map(h => hex(h._1, h._2))
	}
	
	private def correctionsSet(even:Boolean):Set[(Int, Int)] = {
	  even match {
	    case true => Set((0, -1), (1, -1), (-1, 0), (1, 0), (0, 1), (1, 1))
	    case false => Set((-1,-1), (0, -1), (-1, 0), (1, 0), (-1, 1), (0, 1))
	  }
	}
	
	def distance(from:Hex, to:Hex):Int = {
	  val fromCube = from.toCubeHex
	  val toCube = to.toCubeHex
	  (scala.math.abs(fromCube.x - toCube.x) + abs(fromCube.y - toCube.y) + abs(fromCube.z - toCube.z)) / 2;
	}
}