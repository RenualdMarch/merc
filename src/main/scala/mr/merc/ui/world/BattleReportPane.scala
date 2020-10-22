package mr.merc.ui.world

import mr.merc.army.Warrior
import mr.merc.economics.BattleReport
import mr.merc.economics.BattleReport.{Draw, Side1Won, Side2Won}
import mr.merc.local.Localization
import mr.merc.politics.Province
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.control.ScrollPane
import scalafx.Includes._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

class BattleReportPane(battles:List[BattleReport]) extends ScrollPane {
  content = new MigPane {
    battles.foreach { br =>
      add(new OneBattleReportPane(br), "wrap")
    }
  }
  style = Components.largeFontStyle
}

class OneBattleReportPane(battleReport:BattleReport) extends MigPane {

  this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

  private val firstSide = battleReport.side1.map(_.name).mkString(",")
  private val secondSide = battleReport.side2.map(_.name).mkString(",")
  private val warriorColumns = 4

  def province:Province = {
    battleReport.provincesInBattle match {
      case List(p) => p
      case List(first, second) =>
        battleReport.result match {
          case BattleReport.Side1Won =>  second
          case BattleReport.Side2Won => first
          case BattleReport.Draw => first
        }
    }
  }

  add(BigText(Localization("battleReport.vs", firstSide, secondSide)), "wrap, center, span 2")
  add(BigText(Localization("battleReport.battleOf", province.name)), "wrap, center, span 2")
  add(battleReport.result match {
    case Side1Won => BigText(Localization("battleReport.victory", firstSide))
    case Side2Won => BigText(Localization("battleReport.victory", secondSide))
    case Draw => BigText(Localization("battleReport.draw"))
  }, "wrap, center, span 2")

  class WarriorCell(warrior:Warrior, alive:Boolean) extends StackPane {

    this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

    children.addAll(
      Rectangle(72 + 12, 72 + 12, if (alive) Color.White else Color.Red),
      new ImageView(warrior.image)
    )
  }

  private val side1Warriors = battleReport.side1Survived.map { w =>
    new WarriorCell(w, true)
  } ++ battleReport.side1Lost.map { w =>
    new WarriorCell(w, false)
  }

  private val side1MilitiaWarriors = battleReport.side1Militia.map { w =>
    new WarriorCell(w, w.isAlive)
  }

  private val side2MilitiaWarriors = battleReport.side2Militia.map { w =>
    new WarriorCell(w, w.isAlive)
  }

  private val side2Warriors = battleReport.side2Survived.map { w =>
    new WarriorCell(w, true)
  } ++ battleReport.side2Lost.map { w =>
    new WarriorCell(w, false)
  }

  private val constraints = Stream.continually(100d / warriorColumns).take(warriorColumns).toList

  private val side1Regular = GridPaneBuilder.buildWithoutCaption(constraints, side1Warriors)
  private val side2Regular = GridPaneBuilder.buildWithoutCaption(constraints, side2Warriors)
  private val side1Militia = GridPaneBuilder.buildWithoutCaption(constraints, side1MilitiaWarriors)
  private val side2Militia = GridPaneBuilder.buildWithoutCaption(constraints, side2MilitiaWarriors)

  add(new MigPane {
    this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

    add(BigText(Localization("battleReport.regular")), "center, wrap")
    add(side1Regular, "wrap")
    if (side1MilitiaWarriors.nonEmpty) {
      add(BigText(Localization("battleReport.militia")), "center, wrap")
      add(side1Militia, "wrap")
    }
  }, "left, push, grow")

  add(new MigPane {
    this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

    add(BigText(Localization("battleReport.regular")), "center, wrap")
    add(side2Regular, "wrap")
    if (side2MilitiaWarriors.nonEmpty) {
      add(BigText(Localization("battleReport.militia")), "center, wrap")
      add(side2Militia, "wrap")
    }
  }, "right, push, grow")
}
