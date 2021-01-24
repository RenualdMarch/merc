package mr.merc.ui.world

import mr.merc.army.Warrior
import mr.merc.local.Localization
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.control.ScrollPane
import scalafx.Includes._

class SelectedWarriorsPane(warriors: List[Warrior]) extends ScrollPane {

  private val columns = 3
  private val factor = 1d

  style = Components.largeFontStyle

  fitToWidth = true

  content = new MigPane {

    warriors.grouped(columns).foreach { line =>
      val (init, last) = line.splitAt(columns - 1)
      init.foreach(w => add(new WarriorCell(w, true, factor, true)))
      last.foreach(w => add(new WarriorCell(w, true, factor, true), "wrap"))
    }
  }
}

class SelectedWarriorPane(warrior: Warrior) extends MigPane {
  private val imageFactor = 1.5

  add(new WarriorCell(warrior, true, imageFactor), "span 2, center, wrap")
  add(BigText(Localization("diplomacy.state")), "span 2, center, wrap")
  add(new StateComponentColorName(warrior.owner), "span 2, center, wrap")

  add(BigText(Localization("army.warriorType")))
  add(BigText(Localization(warrior.warriorType.name)), "wrap")

  add(BigText(Localization("soldier.competence")))
  add(BigText(EconomicLocalization.localizeWarriorCompetence(warrior.competence)), "wrap")

  add(BigText(Localization("army.hp")))
  add(BigText(s"${warrior.soldier.hp}/${warrior.soldierType.hp}"), "wrap")

  add(BigText(Localization("army.warrior.supply")))
  add(BigText(DoubleFormatter().format(warrior.historicalNeeds.last.needsReceivedPercentage * 100) + "%"), "wrap")
}