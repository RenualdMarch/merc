package mr.merc.map.pathfind

import scala.collection.mutable.ArrayBuffer

class Node[T](val t:T, val selfPrice:Double, var parent:Option[Node[T]]) {
     
     def path:List[Node[T]] = {
       var result = ArrayBuffer(this)
       var current = parent
       while (current.isDefined) {
         val next = current.get
         result += next
         current = next.parent
       }
       result.toList.reverse
     } 
      
     def pathPrice = path.map(_.selfPrice).sum
     
     override def equals(any:Any):Boolean = {
      any match {
    	case node:Node[T] => node.t == t
    	case _ => false
      }
    }
  
    override def hashCode() = t.hashCode
  }	  