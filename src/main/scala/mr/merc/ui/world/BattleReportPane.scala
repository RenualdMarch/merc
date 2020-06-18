package mr.merc.ui.world

import mr.merc.army.Warrior
import mr.merc.economics.BattleReport
import mr.merc.economics.BattleReport.{Draw, Side1Won, Side2Won}
import mr.merc.local.Localization
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.control.ScrollPane
import scalafx.Includes._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.{BorderPane, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

class BattleReportPane(battles:List[BattleReport]) extends ScrollPane {
  content = new MigPane {
    battles.foreach { br =>
      add(new OneBattleReportPane(br), "wrap")
    }
  }
  fitToWidth = true
  style = Components.largeFontStyle
}

class OneBattleReportPane(battleReport:BattleReport) extends MigPane {

  private val firstSide = battleReport.side1.map(_.name).mkString(",")
  private val secondSide = battleReport.side2.map(_.name).mkString(",")
  private val warriorColumns = 4

  add(BigText(Localization("battleReport.vs", firstSide, secondSide)), "wrap")
  add(battleReport.result match {
    case Side1Won => BigText(Localization("battleReport.victory", firstSide))
    case Side2Won => BigText(Localization("battleReport.victory", secondSide))
    case Draw => BigText(Localization("battleReport.draw"))
  }, "wrap")

  class WarriorCell(warrior:Warrior, alive:Boolean) extends StackPane {
    children.addAll(
      Rectangle(72, 72, if (alive) Color.Transparent else Color.Red),
      new ImageView(warrior.image)
    )
  }

  private val side1Warriors = battleReport.side1Survived.map { w =>
    new WarriorCell(w, true)
  } ++ battleReport.side1Lost.map { w =>
    new WarriorCell(w, false)
  }

  private val side2Warriors = battleReport.side2Survived.map { w =>
    new WarriorCell(w, true)
  } ++ battleReport.side2Lost.map { w =>
    new WarriorCell(w, false)
  }

  private val constraints = Stream.continually(100d / warriorColumns).take(warriorColumns).toList

  private val side1 = GridPaneBuilder.buildWithoutCaption(constraints, side1Warriors)
  private val side2 = GridPaneBuilder.buildWithoutCaption(constraints, side2Warriors)

  add(side1, "left")
  add(side2, "right")
}
