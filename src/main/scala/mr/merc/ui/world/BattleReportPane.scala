package mr.merc.ui.world

import mr.merc.army.Warrior
import mr.merc.economics.{Battle, BattleReport}
import mr.merc.economics.BattleReport.{Draw, Side1Won, Side2Won}
import mr.merc.local.Localization
import mr.merc.politics.{Province, State}
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.control.ScrollPane
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.Node
import scalafx.scene.image.ImageView
import scalafx.scene.layout.{HBox, Pane, Region, StackPane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.{Line, Rectangle}

class BattleReportPane(battles:List[BattleReport]) extends ScrollPane {
  content = new MigPane {
    battles.foreach { br =>
      add(new OneBattleReportPane(br), "wrap")
    }
  }
  style = Components.largeFontStyle
}

class OneBattleReportPane(battleReport:BattleReport) extends MigPane {

  private val factor = 1d

  this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

  private def firstSide = battleReport.side1.map(x => new StateComponentColorName(x))
  private def secondSide = battleReport.side2.map(x => new StateComponentColorName(x))
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

  add(new HBox {
    firstSide.foreach(x => children.add(x))
    children.add(BigText(Localization("battleReport.vs")))
    secondSide.foreach(x => children.add(x))
    alignment = Pos.Center
  }.delegate, "center, wrap, span 2")
  add(BigText(Localization("battleReport.battleOf", province.name)), "wrap, center, span 2")
  add(battleReport.result match {
    case Side1Won =>
      new HBox {
        children.add(BigText(Localization("battleReport.victory")))
        firstSide.foreach(x => children.add(x))
        alignment = Pos.Center
      }
    case Side2Won =>
      new HBox {
        children.add(BigText(Localization("battleReport.victory")))
        secondSide.foreach(x => children.add(x))
        alignment = Pos.Center
      }
    case Draw => BigText(Localization("battleReport.draw"))
  }, "wrap, center, span 2")

  private val side1Warriors = battleReport.side1Survived.map { w =>
    new WarriorCell(w, true, factor)
  } ++ battleReport.side1Lost.map { w =>
    new WarriorCell(w, false, factor)
  }

  private val side1MilitiaWarriors = battleReport.side1Militia.map { w =>
    new WarriorCell(w, w.isAlive, factor)
  }

  private val side2MilitiaWarriors = battleReport.side2Militia.map { w =>
    new WarriorCell(w, w.isAlive, factor)
  }

  private val side2Warriors = battleReport.side2Survived.map { w =>
    new WarriorCell(w, true, factor)
  } ++ battleReport.side2Lost.map { w =>
    new WarriorCell(w, false, factor)
  }

  private val constraints = Stream.continually(100d / warriorColumns).take(warriorColumns).toList

  private val side1Regular = GridPaneBuilder.buildWithoutCaption(constraints, side1Warriors)
  private val side2Regular = GridPaneBuilder.buildWithoutCaption(constraints, side2Warriors)
  private val side1Militia = GridPaneBuilder.buildWithoutCaption(constraints, side1MilitiaWarriors)
  private val side2Militia = GridPaneBuilder.buildWithoutCaption(constraints, side2MilitiaWarriors)

  add(new MigPane {
    this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

    if (side1Warriors.nonEmpty) {
      add(BigText(Localization("battleReport.regular")), "center, wrap")
      add(side1Regular, "wrap")
    }

    if (side1MilitiaWarriors.nonEmpty) {
      add(BigText(Localization("battleReport.militia")), "center, wrap")
      add(side1Militia, "wrap")
    }
  }, "left, push, grow")

  add(new MigPane {
    this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"
    if (side2Warriors.nonEmpty) {
      add(BigText(Localization("battleReport.regular")), "center, wrap")
      add(side2Regular, "wrap")
    }
    if (side2MilitiaWarriors.nonEmpty) {
      add(BigText(Localization("battleReport.militia")), "center, wrap")
      add(side2Militia, "wrap")
    }
  }, "right, push, grow")
}

class WarriorCell(warrior:Warrior, alive:Boolean, factor: Double, showHpPercentage: Boolean = false) extends StackPane {



  this.style = "-fx-border-color: black;-fx-border-width: 1;-fx-border-insets: 3px; -fx-background-insets: 3px;"

  children.addAll(
    Rectangle(72 * factor + 12, 72 * factor + 12, if (alive) Color.White else Color.Red),
    new ImageView(warrior.image(factor))
  )

  if (showHpPercentage) {
    val actualLength = (factor * 72 + 12) * warrior.hpPercentage
    val line = new Line() {
      startX = 0
      startY = 72 * factor + 10
      endX = actualLength
      endY = 72 * factor + 10

      strokeWidth = 4
      stroke = Color.Red
    }
    children.add(line)
    StackPane.setAlignment(line, Pos.BottomLeft)
  }
}

class BeforeBattleDialog(battle: Battle) extends DialogStage[Boolean] {

  lazy val playBattleButton = new BigButton {
    text = Localization("battleDialog.playBattle")
    onAction = { _ =>
      dialogResult = Some(true)
      close()
    }
  }

  lazy val autoBattleButton = new BigButton {
    text = Localization("battleDialog.autoBattle")
    onAction = { _ =>
      dialogResult = Some(false)
      close()
    }
  }

  override def additionalButtons = List(playBattleButton, autoBattleButton)

  private val warriorColumns = 4
  private val constraints = Stream.continually(100d / warriorColumns).take(warriorColumns).toList

  private def statePane(state: State, warriors: List[Warrior]): MigPane = new MigPane {
    add(new StateComponentColorName(state), s"center, wrap")

    val pane = GridPaneBuilder.buildWithoutCaption(constraints,
      warriors.map(w => new WarriorCell(w, true, 1d)))

    add(pane)
  }

  private def buildAllStatesPanel(states: List[State], map:Map[State, List[Warrior]]): Region = new ScrollPane {
    fitToWidth = true

    content = new MigPane {
      states.foreach { st =>
        add(statePane(st, map(st)), "wrap")
      }
    }
  }

  override protected def dialogContent: Region = {
    val pane = new PaneWithTwoVerticalChildren()

    val warriors = battle.allWarriors ::: battle.allMilitia

    val (left, right) = battle.sides

    val map = warriors.groupBy(_.owner)
    pane.setTwoChildren(buildAllStatesPanel(left.toList, map), buildAllStatesPanel(right.toList, map))
    pane
  }

  override protected def shouldAddCancelButton: Boolean = false

  override protected def shouldAddOkButton: Boolean = false

  override protected def css: Option[String] = None
}