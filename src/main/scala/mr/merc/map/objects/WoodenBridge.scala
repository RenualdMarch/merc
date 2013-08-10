package mr.merc.map.objects

import mr.merc.map.hex.Directions
import mr.merc.map.hex.TerrainHex
import mr.merc.image.MImage
import scala.math.Ordering
import mr.merc.map.hex.TerrainHexField

object WoodenBridge extends MapObject("woodenBridge") {
	private val endsMap = Directions.values.map(dir => (dir, "end-" + dir.toString().toLowerCase())).toMap 
    private val centersMap = List((Directions.N, Directions.S), (Directions.NE, Directions.SW), (Directions.SE, Directions.NW)).
    							map(pair => (pair, pair._1.toString().toLowerCase() + "-" + 
    							    pair._2.toString().toLowerCase())).toMap
    private val selectionPriority = List((Directions.NE, Directions.SW), (Directions.SE, Directions.NW), (Directions.N, Directions.S))
    							    
    override def images(hex:TerrainHex, field:TerrainHexField):List[MImage] = {
	  val neighbours = field.neighboursWithDirections(hex.x, hex.y)
	  hex.mapObj match {
	  	case Some(WoodenBridge) => imagesForWoodenBridge(neighbours)
	  	case Some(_) | None => imagesForWoodenBridgeNeighbour(hex, field)
	  }
	}
	
	private def imagesForWoodenBridge(neighbours:Map[Directions.Direction, TerrainHex]):List[MImage] = {
	  val neigBridges = neighbours.filter(pair => pair._2.mapObj == Some(WoodenBridge))

	  if (neigBridges.isEmpty) {
	    centersMap.get(selectionPriority(0)).map(n => MImage(imagePath(n))).toList
	  } else if (neigBridges.size == 1) {
	    val pair = neigBridges.toList(0)
	    val dir = pair._1
	    val name = centersMap.find(p => p._1._1 == dir || p._1._2 == dir).get._2
	    List(MImage(imagePath(name)))
	  } else {	 
	    val name = centersMap(findBridgeDirection(neigBridges))
	    List(MImage(imagePath(name)))
	  }
	}
	
	private def findBridgeDirection(neigBridges:Map[Directions.Direction,TerrainHex]):Directions.DirPair = {
	  val list = neigBridges.keys.toList
	  val centers = centersMap.keys.toList
	  val weighted = centers.map(pair => (pair, (list.toSet & Set(pair._1, pair._2)).size))
	  val sorted = weighted.sortWith((first:(Directions.DirPair, Int), second:(Directions.DirPair, Int)) => {
	    if (first._2 > second._2) {
	      true
	    } else if (first._2 < second._2) {
	      false
	    } else {
	      val firstPos = selectionPriority.indexOf(first._1)
	      val secondPos = selectionPriority.indexOf(second._1)
	      firstPos < secondPos
	    }
	  })
	  
	  sorted(0)._1
	}
	
	private def imagesForWoodenBridgeNeighbour(hex:TerrainHex, field:TerrainHexField):List[MImage] = {
	  val neighbours = field.neighboursWithDirections(hex.x, hex.y).
						filter(pair => pair._2.mapObj == Some(WoodenBridge))
	  val names = for (pair <- neighbours) yield {
		  val neigHex = pair._2
		  val dir = pair._1
		  val neigBridges = field.neighboursWithDirections(neigHex.x, neigHex.y).
		  			filter(pair => pair._2.mapObj == Some(WoodenBridge))
		  val bridgeDirection = findBridgeDirection(neigBridges)
		  if (bridgeDirection._1 != dir && bridgeDirection._2 != dir) {
		    None
		  } else {
		    Some(endsMap(dir))
		  }
		}
		
		names.flatten.map(n => MImage(imagePath(n))).toList
	}
}