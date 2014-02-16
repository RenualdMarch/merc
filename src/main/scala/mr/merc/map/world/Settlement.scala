package mr.merc.map.world

import javax.xml.bind.annotation.XmlRootElement
import javax.xml.bind.annotation.XmlAccessorType
import javax.xml.bind.annotation.XmlAccessType
import javax.xml.bind.annotation.XmlElement
import javax.xml.bind.annotation.XmlType
import javax.xml.bind.JAXBContext
import collection.JavaConversions._
import javax.xml.bind.annotation.XmlAttribute
import scala.beans.BeanProperty
import scala.xml.XML

object Settlement {
  def loadSettlements(fileName: String): Map[(Int, Int), Settlement] = {
    val xml = XML.load(getClass.getResourceAsStream("/maps/" + fileName + ".xml"))

    val settlements = for (node <- xml \ "settlement") yield {
      val x = (node \ "@x").toString().toInt
      val y = (node \ "@y").toString().toInt
      val nameKey = (node \ "@nameKey").toString()
      val cultureName = (node \ "@cultureName").toString()
      val population = (node \ "@population").toString().toInt

      ((x, y), Settlement(nameKey, cultureName, population))
    }
    settlements toMap
  }
}

case class Settlement(nameKey: String, cultureName: String, population: Int) {
  def picturePath {
    val prefix = "/images/cities/" + cultureName + "/"

    val name = population match {
      case p if 0 until 100 contains p => "tiny"
      case p if 100 until 500 contains p => "small"
      case p if 500 until 2000 contains p => "average"
      case p if 2000 until 5000 contains p => "huge"
      case p => "enormous"
    }

    prefix + name + ".png"
  }
}
