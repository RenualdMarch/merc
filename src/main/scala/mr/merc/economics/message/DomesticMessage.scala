package mr.merc.economics.message

import mr.merc.politics.Official
import scalafx.scene.layout.Region

trait DomesticMessage {
  def from:Official
  def title:String
  def body: Region
}

abstract class InformationDomesticMessage(val from:Official, val title:String) extends DomesticMessage