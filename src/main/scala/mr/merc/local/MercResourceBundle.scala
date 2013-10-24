package mr.merc.local

import java.util.ResourceBundle
import scala.collection.JavaConversions._
import java.util.Collections

class MercResourceBundle(locale:String) extends ResourceBundle {
  private val map = Localization.messages(locale)
  def getKeys(): java.util.Enumeration[String] = Collections.enumeration(map.keys)
  def handleGetObject(x:String):AnyRef = map(x)
  
}