package mr.merc.economics.message

import scalafx.scene.layout.{Pane, Region}

trait DomesticMessage {
  def from:String
  def title:String
  def body:Region
}

class InformationDomesticMessage(val from:String, val title:String, val body:Region) extends DomesticMessage