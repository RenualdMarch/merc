package mr.merc.ui.world

import javafx.event.{Event, EventHandler}
import mr.merc.local.Localization
import scalafx.scene.control.{Menu, MenuBar, MenuItem}


import scala.collection.JavaConverters._

class WorldMenu(parent: WorldFrame) extends MenuBar {
  this.stylesheets.add("/css/worldMenu.css")

  val politicsMenu = new Menu(Localization("menu.domesticPolicy"))
  val diplomacyMenu = new Menu(Localization("menu.foreignPolicy"))
  val gameMenu = new Menu(Localization("menu.game"))
  val viewMenu = new Menu(Localization("menu.view"))

  this.menus.addAll(politicsMenu, diplomacyMenu/*, viewMenu*/, gameMenu)

  val army = new MenuItem(Localization("menu.defence"))
  val relations = new MenuItem(Localization("menu.relations"))
  relations.onAction = {_ =>
    parent.showDiplomacyPane()
  }
  val emailMenu = new MenuItem(Localization("menu.mail"))
  emailMenu.onAction = { _ =>
    parent.showMailPane()
  }
  val allWars = new MenuItem(Localization("menu.allWars"))
  allWars.onAction = { _ =>
    parent.showWarsPane()
  }

  val foreignTradeMenu = new MenuItem(Localization("menu.foreignTrade"))
  foreignTradeMenu.onAction = { _ =>
    parent.showForeignTradePane()
  }

  diplomacyMenu.items.addAll(emailMenu, relations, allWars, foreignTradeMenu, army)

  val budgetMenu = new MenuItem(Localization("menu.budget"))
  budgetMenu.onAction = { _ =>
    parent.showBudgetPane()
  }

  val parliamentMenu = new MenuItem(Localization("menu.parliament"))
  parliamentMenu.onAction = {_ =>
    parent.showParliamentPane()
  }

  politicsMenu.items.addAll(budgetMenu, parliamentMenu)

  val saveMenu = new MenuItem(Localization("menu.save")) {
    onAction = {_ =>
      parent.saveGame()
    }
  }
  val loadMenu = new MenuItem(Localization("menu.load")) {
    onAction = {_ =>
      parent.sceneManager.loadGame()
    }
  }
  val options = new MenuItem(Localization("menu.options"))
  val exit = new MenuItem(Localization("menu.exit"))
  gameMenu.items.addAll(saveMenu, loadMenu, options, exit)

  val hideMinimap = new ToggleMenuItem(
    x => if (x) parent.hideMinimap() else parent.showMinimap(),
    x => if (x) Localization("menu.showMinimap") else Localization("menu.hideMinimap"),
    false
  )
  viewMenu.items.addAll(hideMinimap)

  this.lookupAll(".menu-item").asScala.foreach {n =>
    n.setStyle(s"-fx-font-size: ${Components.largeFontSize};")
  }

  this.lookupAll(".menu-bar").asScala.foreach {n =>
    n.setStyle(s"-fx-font-size: ${Components.largeFontSize};")
  }
}

class ToggleMenuItem(f:Boolean => Unit, label: Boolean => String, initalState:Boolean) extends MenuItem {
  private var currentState = initalState

  this.text = label(initalState)

  this.onAction = {_ =>
    currentState = !currentState
    this.text = label(currentState)
    f(currentState)
  }
}