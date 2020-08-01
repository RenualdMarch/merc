package mr.merc.economics.message

import scalafx.scene.layout.Region

trait DomesticMessage {
  def from:String
  def title:String
  def body: Region
}

abstract class InformationDomesticMessage(val from:String, val title:String) extends DomesticMessage