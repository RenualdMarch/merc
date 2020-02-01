package mr.merc.economics.message

import scalafx.scene.layout.Pane

trait DomesticMessage {
  def from:String
  def title:String
  def body:Pane
}

class InformationDomesticMessage(val from:String, val title:String, val body:Pane) extends DomesticMessage