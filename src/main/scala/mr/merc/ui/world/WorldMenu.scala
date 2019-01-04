package mr.merc.ui.world

import mr.merc.local.Localization
import scalafx.scene.control.{Menu, MenuBar, MenuItem}

class WorldMenu(parent: WorldFrame) extends MenuBar {
  this.style = s"-fx-font-size: ${Components.mediumFontSize}"

  val politicsMenu = new Menu(Localization("menu.domesticPolicy"))
  val diplomacyMenu = new Menu(Localization("menu.foreignPolicy"))
  val armyMenu = new Menu(Localization("menu.defence"))
  val gameMenu = new Menu(Localization("menu.game"))
  val viewMenu = new Menu(Localization("menu.view"))

  this.menus.addAll(politicsMenu, diplomacyMenu, armyMenu, viewMenu, gameMenu)

  val budgetMenu = new MenuItem(Localization("menu.budget"))
  val parliament = new MenuItem(Localization("menu.parliament"))
  politicsMenu.items.addAll(budgetMenu, parliament)

  val saveMenu = new MenuItem(Localization("menu.save"))
  val loadMenu = new MenuItem(Localization("menu.load"))
  val options = new MenuItem(Localization("menu.options"))
  val exit = new MenuItem(Localization("menu.exit"))
  gameMenu.items.addAll(saveMenu, loadMenu, options, exit)

  val hideMinimap = new ToggleMenuItem(
    x => if (x) parent.hideMinimap() else parent.showMinimap(),
    x => if (x) Localization("menu.showMinimap") else Localization("menu.hideMinimap"),
    false
  )
  viewMenu.items.addAll(hideMinimap)

}

class ToggleMenuItem(f:Boolean => Unit, label: Boolean => String, initalState:Boolean) extends MenuItem {
  private var currentState = initalState

  this.text = label(initalState)

  this.onAction = {e =>
    currentState = !currentState
    this.text = label(currentState)
    f(currentState)
  }
}