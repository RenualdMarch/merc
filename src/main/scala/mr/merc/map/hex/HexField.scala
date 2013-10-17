package mr.merc.map.hex

import scala.math._
import mr.merc.map.Grid
import scala.reflect.ClassTag

class HexField[T <: Hex : ClassTag](val width:Int, val height:Int, init:(Int, Int) => T) extends Grid[T] {
	private val arr = Array.ofDim[T](width, height)
	for (x <- 0 until width; y <- 0 until height) {
	  if (isLegalCoords(x, y)) { 
		  arr(x)(y) = init(x, y)
	  }
	}
	
	def hexes:Seq[T] = {
	  val indices = for (y <- 0 until height; x <- 0 until width) yield {
		  (x, y)
	  }
	  
	  indices.filter(i => isLegalCoords(i._1, i._2)).map(i => hex(i._1, i._2))
	}
	
	def isLegalCoords(x:Int, y:Int):Boolean = {
	  if (x % 2 == 1 && y == height - 1) {
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
	
	def neighbours(hex:T):Set[T] = neighbours(hex.x, hex.y)
	
	def neighbours(x:Int, y:Int):Set[T] = neighboursList(x, y).toSet
	
	private def neighboursList(x:Int, y:Int):List[T] = {
		val allNeighboursCoords = neighboursListWithInvalid(x, y)
		val correctCoords = allNeighboursCoords.filter(h => isLegalCoords(h._1, h._2))
		correctCoords.map(h => hex(h._1, h._2))
	}
	
	private def neighboursListWithInvalid(x:Int, y:Int):List[(Int, Int)] = {
	  	// x % 2 == 1 is even because we start from zero
		val corrections = correctionsList(x % 2 == 1)
		corrections.map(h => (h._1 + x, h._2 + y))
	}
		
	def neighboursWithDirections(hex:T):Map[Direction, T] = neighboursWithDirections(hex.x, hex.y)
	
	def neighboursWithDirections(x:Int, y:Int):Map[Direction, T] = {
	  val resultList = (directionsList zip neighboursListWithInvalid(x, y)).filter(dh => isLegalCoords(dh._2._1, dh._2._2))
	  resultList.map(df => (df._1, hex(df._2._1, df._2._2))).toMap
	}
		
	private def correctionsList(even:Boolean):List[(Int, Int)] = {
	  even match {
	    case true => List((-1, 0), (0, -1), (1, 0), (1, 1), (0, 1), (-1, 1))
	    case false => List((-1, -1), (0, -1), (1, -1), (1, 0), (0, 1), (-1, 0))
	  }
	}
	
	private val directionsList = List(NW, N, NE, SE, S, SW)
		
	def distance(from:T, to:T):Int = {
	  val fromCube = from.toCubeHex
	  val toCube = to.toCubeHex
	  (scala.math.abs(fromCube.x - toCube.x) + abs(fromCube.y - toCube.y) + abs(fromCube.z - toCube.z)) / 2;
	}
}