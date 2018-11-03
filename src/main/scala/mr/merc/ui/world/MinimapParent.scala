package mr.merc.ui.world

import mr.merc.ui.minimap.Minimap
import org.tbee.javafx.scene.layout.MigPane
import scalafx.Includes._

class MinimapParent(child:Minimap) extends MigPane("", "5px[]5px", "5px[]5px") {
  this.styleClass.add("interfacePane")
  this.stylesheets.add("/css/worldPane.css")

  add(child, "grow,push")
}
