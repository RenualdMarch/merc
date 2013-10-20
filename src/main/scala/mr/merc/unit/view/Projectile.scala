package mr.merc.unit.view

import mr.merc.map.hex.Direction
import mr.merc.map.hex._
import mr.merc.image.MImage
import mr.merc.map.hex.view.TerrainHexView
import scala.xml.XML
import java.io.File
import scala.xml.Node
import scala.xml.NodeSeq

object Projectile {
  private val rootPath = "/images/projectiles/"
  
  private val map = parseProjectiles

  def apply(name:String) = map(name)
    
  private def parseProjectiles:Map[String, Projectile] = {
    val path = getClass.getResource("/conf/projectiles.xml").toURI
    val xml = XML.loadFile(new File(path))
    val parsed = (xml \ "projectile") map (node => {
      val name = (node \ "@name").toString()
      
      val move = parseState(name, node \ "move")
      val startTemp = parseState(name, node \ "start")
      val endTemp = parseState(name, node \ "end")
      val start = if(startTemp.values.forall(_.isEmpty)) firstIndicesOfLists(move) else startTemp 
      val end = if (endTemp.values.forall(_.isEmpty)) lastIndicesOfLists(move) else endTemp
      
      new Projectile(name, start, move, end)
    })
    
    parsed map (p => (p.name, p)) toMap
  }
  
  private def firstIndicesOfLists(map:Map[Direction, List[MImage]]):Map[Direction, List[MImage]] = {
    map map (kv => (kv._1, List(kv._2.head)))
  }
  
  private def lastIndicesOfLists(map:Map[Direction, List[MImage]]):Map[Direction, List[MImage]] = {
    map map (kv => (kv._1, List(kv._2.last)))
  }
  
  private def parseImagesList(projectileName:String, node:NodeSeq):List[MImage] = {
    (node \ "image" map (parseImage(projectileName, _))).toList    
  }
  
  private def parseImage(projectileName:String, node:Node):MImage = {
    val name = (node \ "@name").toString()    
    MImage(rootPath + projectileName + "/" + name + ".png")
  }
  
  private def parseState(name:String, node:NodeSeq):Map[Direction, List[MImage]] = {
      val all = parseImagesList(name, node \ "all")
      val n = parseImagesList(name, node \ "n")
      val ne = parseImagesList(name, node \ "ne")
      
      if (!all.isEmpty) {
        Direction.list map ((_ -> all)) toMap
      } else {
        val nw = ne map (_.mirrorVertically)
        val sw = nw map (_.mirrorHorizontally)
        val s = n map (_.mirrorHorizontally)
        val se = ne map (_.mirrorHorizontally)
        Map((N -> n), (NE -> ne), (NW -> nw), (SW -> sw), (S -> s), (SE -> se))
      }
  }
}

// maps cann't be empty
class Projectile(val name:String, val start:Map[Direction, List[MImage]], 
    val move:Map[Direction, List[MImage]],
    val end:Map[Direction, List[MImage]]) {
    private val speed = 100
    // 6 is directions count
	require(start.size == 6)
	require(move.size == 6)
	require(end.size == 6)
	
	def buildView(dir:Direction, from:(Int, Int), to:(Int, Int)) = 
	  new ProjectileView(start(dir), move(dir), end(dir),
	    from, to, speed)
}