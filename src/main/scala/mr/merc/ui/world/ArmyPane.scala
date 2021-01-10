package mr.merc.ui.world

import mr.merc.army.{Warrior, WarriorType}
import mr.merc.local.Localization
import mr.merc.map.hex.view.ProvinceView
import mr.merc.politics.{Province, State}
import scalafx.collections.ObservableBuffer
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{BorderPane, Pane, Region}
import mr.merc.util.FxPropertyUtils._
import scalafx.beans.property.{ObjectProperty, ReadOnlyObjectProperty}
import EconomicLocalization._
import javafx.scene.control.TableCell
import mr.merc.army.WarriorCompetence
import mr.merc.economics._
import org.tbee.javafx.scene.layout.MigPane
import scalafx.Includes._
import scalafx.scene.control.TabPane.TabClosingPolicy
import scalafx.scene.input.{ClipboardContent, TransferMode}
import scalafx.stage.Stage
import mr.merc.ui.dialog.ModalDialog
import mr.merc.ui.world.SelectRecruitWarriorDialog.RecruitWarriorOrder
import ModalDialog._
import mr.merc.map.terrain._
import mr.merc.unit._
import Localization._
import javafx.geometry.Pos
import mr.merc.image.MImage
import mr.merc.unit.view.AttackView
import scalafx.scene.chart.{AreaChart, LineChart, NumberAxis, XYChart}


class ArmyPane(province: Province, provinceView: ProvinceView, worldState: WorldState, stage: Stage) extends BorderPane {
  private val controller = new ArmyPaneController(province, provinceView, worldState)

  def soldiersWhereChanged: Boolean = controller.soldiersWhereChanged

  private val tabPane = new TabPane() {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  private val movementPane = new Tab {
    text = Localization("army.movement")
    style = Components.largeFontStyle
    content = new ArmyMovementPane(controller, province)
  }

  private val supplyPane = new Tab {
    text = Localization("army.supply")
    style = Components.largeFontStyle
    content = new ArmySupplyPane(controller, stage)
  }

  tabPane.tabs.addAll(movementPane, supplyPane)
  center = tabPane
}

class ArmySupplyPane(controller: ArmyPaneController, stage: Stage) extends PaneWithTwoHorizontalChildren(0.4) {
  val warriorsTable = new WarriorsTablePane(controller)
  val recruitmentPane = new WarriorRecruitmentPane(controller, stage)
  val leftPane = PaneWithTwoVerticalChildren(warriorsTable, recruitmentPane, 0.6)
  val eitherProperty = bindTwoProperties(warriorsTable.selectedWarrior, recruitmentPane.projectsPane.selectedRow)

  private def f(either: Either[Warrior, BusinessProject]) = Option(either) match {
    case Some(Left(warrior)) =>
      recruitmentPane.projectsPane.projectsTable.delegate.getSelectionModel.clearSelection()
      new WarriorInfoPane(warrior).delegate
    case Some(Right(project)) =>
      warriorsTable.table.delegate.getSelectionModel.clearSelection()
      new ProjectPane(project)
    case None => new MigPane() with WorldInterfaceJavaNode
  }

  val rightPane = new PropertyDependentPane[Either[Warrior, BusinessProject]](eitherProperty, f)
  setTwoChildren(leftPane, rightPane)
}

class WarriorsTablePane(controller: ArmyPaneController) extends BorderPane with WorldInterfaceNode {
  val table = new TableView[Warrior] {
    style = Components.mediumFontStyle
  }

  val soldierImageColumn = new TableColumn[Warrior, Image] {
    text = Localization("army.warrior")
    cellValueFactory = p => ObjectProperty(p.value.image(1d))
    cellFactory = p => new TableCell[Warrior, Image] {
      override def updateItem(t: Image, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(new ImageView(t))
      }
    }
    editable = false
  }

  val soldierTypeColumn = new TableColumn[Warrior, String] {
    text = Localization("army.warriorType")
    cellValueFactory = p => ObjectProperty(localizeWarriorType(p.value))
    editable = false
  }

  val soldierHP = new TableColumn[Warrior, String] {
    text = Localization("army.hp")
    cellValueFactory = p => ObjectProperty(IntFormatter().format(p.value.hpPercentage * 100) + "%")
    editable = false
  }

  private val disposeButtonColumn = new TableColumn[Warrior, Button] {
    text = Localization("disposeSoldier")
    cellFactory = p => new TableCell[Warrior, Button] {

      def button: Button = {
        val w = getTableView.getItems.get(getIndex)
        new MediumButton() {
          text = Localization("dispose")
          onAction = { _ =>
            controller.disposeSoldier(w)
          }
        }
      }

      override def updateItem(t: Button, b: Boolean): Unit = {
        super.updateItem(t, b)
        if (b) {
          setGraphic(null)
        } else {
          setGraphic(button)
        }
      }
    }
    editable = false
  }

  table.columns ++= List(soldierImageColumn, soldierTypeColumn, soldierHP, disposeButtonColumn)
  table.items = controller.allSoldiers
  table.delegate.getSelectionModel.clearAndSelect(0)

  center = table

  val selectedWarrior: ReadOnlyObjectProperty[Warrior] = table.getSelectionModel.selectedItemProperty()

  def warriorsTable: TableView[Warrior] = table
}

class WarriorRecruitmentPane(controller: ArmyPaneController, stage: Stage) extends MigPane {
  val projectsPane = new RegionProjectsPane(controller.recruitProjects)
  val recruitWarriorButton = BigButton(Localization("army.recruit"))
  recruitWarriorButton.disable = !controller.canRecruit
  recruitWarriorButton.onAction = { _ =>
    val dialog = new SelectRecruitWarriorDialog(stage, controller.possibleWarriorToRecruit())
    dialog.showDialog(stage)
    dialog.dialogResult.foreach(_.foreach { case RecruitWarriorOrder(wt, wc, c, st, count) =>
      for {
        _ <- 0 until count
      } {
        controller.recruitWarrior(wt, wc, c)
      }
    })
  }

  add(projectsPane, "grow, push, wrap")
  add(recruitWarriorButton, "center")
}

class WarriorInfoPane(warrior: Warrior) extends BorderPane {
  private val tabPane = new TabPane() {
    style = Components.mediumFontStyle
    tabClosingPolicy = TabClosingPolicy.Unavailable
  }

  private val supplyPane = new Tab {
    text = Localization("army.supplyHistory")
    style = Components.largeFontStyle
    content = new WarriorSupplyPane(warrior)
  }

  private val soldierTypePane = new Tab {
    text = Localization("army.soldierType")
    style = Components.largeFontStyle
    content = new WarriorTypeInfoPane(warrior)
  }

  tabPane.tabs.addAll(supplyPane, soldierTypePane)
  center = tabPane
}

class WarriorSupplyPane(warrior: Warrior) extends MigPane {
  def buildSupplyChart(): Option[LineChart[Number, Number]] = {
    val size = warrior.historicalNeeds.size
    warrior.historicalNeeds.headOption.map { first =>
      val xAxis = new NumberAxis(first.turn, first.turn + size - 1, 1)
      val yAxis = new NumberAxis()
      xAxis.label = Localization("weeks")
      yAxis.label = Localization("units")

      val chart = new LineChart[Number, Number](xAxis, yAxis)
      chart.title = Localization("production")
      chart.style = Components.mediumFontStyle

      val needed = new XYChart.Series[Number, Number]()
      needed.name = Localization("soldier.needed")

      val received = new XYChart.Series[Number, Number]()
      received.name = Localization("soldier.received")

      val productsMap = first.demanded.keySet.map { p =>
        val series = new XYChart.Series[Number, Number]()
        series.name = EconomicLocalization.localizeProduct(p)
        p -> series
      }.toMap

      warrior.historicalNeeds.foreach { n =>
        needed.getData.add(XYChart.Data[Number, Number](n.turn, n.demanded.values.sum))
        received.getData.add(XYChart.Data[Number, Number](n.turn, n.received.values.sum))
        n.received.foreach { case (p, c) =>
          productsMap(p).getData.add(XYChart.Data[Number, Number](n.turn, c))
        }
      }

      chart.legendVisible = true
      chart.getData.addAll(needed, received)
      productsMap.values.foreach { v =>
        chart.getData.add(v)
      }

      chart
    }
  }

  buildSupplyChart().foreach { ch =>
    add(ch, "grow, push")
  }
}

class WarriorTypeInfoPane(example: Warrior) extends MigPane("") with WorldInterfaceJavaNode {

  private val st = example.soldierType

  val defenceAndResistances: Pane = {
    val migPane = new MigPane("wrap 6")
    val pairs = List[(DefenceType, TerrainKind, String)](
      (WaterDefence, WaterKind, "coast"),
      (ForestDefence, ForestKind, "forest"),
      (SwampDefence, SwampKind, "swamp"),
      (HillDefence, HillKind, "hills"),
      (MountainDefence, MountainKind, "mountains"),
      (SandDefence, SandKind, "desert"),
      (GrassDefence, GrassKind, "grass"),
      (BuildingDefence, WallsKind, "village"),
      (SnowDefence, SnowKind, "snow"),
      (IceDefence, IceKind, "ice"))

    List("soldier.terrain", "soldier.defence", "soldier.movement",
      "soldier.terrain", "soldier.defence", "soldier.movement").foreach { k =>
      migPane.add(MediumText(Localization(k)))
    }
    pairs.foreach { case (dt, tk, tt) =>
      migPane.add(new ImageView(MImage(s"/images/ui/terrainTypes/$tt.png").image))
      migPane.add(BigText(st.defence(dt).toString + "%"), "center")
      migPane.add(BigText(st.moveCost(tk).toString), "center")
    }
    migPane
  }

  val mainInfo: Pane = {
    val migPane = new MigPane("wrap 2")
    migPane.add(MediumText(Localization("soldier.type")))
    migPane.add(MediumText(Localization(st.name)))
    migPane.add(MediumText(Localization("soldier.competence")))
    migPane.add(MediumText(localizeWarriorCompetence(example.competence)))
    migPane.add(MediumText(Localization("soldier.hp")))
    migPane.add(MediumText(st.hp.toString))
    migPane.add(MediumText(Localization("soldier.moves")))
    migPane.add(MediumText(st.movement.toString))
    migPane.add(MediumText(Localization("soldier.abilities")))
    if (st.attributes.nonEmpty) {
      migPane.add(MediumText(st.attributes.map(at => Localization(at.productPrefix)).mkString(", ")))
    } else {
      migPane.add(MediumText(Localization("soldier.abilities.none")))
    }

    migPane
  }

  val attacksInfo: Pane = {
    val migPane = new MigPane("wrap 5")
    val viewInfo = example.soldierView(1d, false, false).viewInfo
    (st.attacks zip viewInfo.attacks).foreach { case (at, atView) =>
      migPane.add(new ImageView(atView.attackImage))
      migPane.add(MediumText(Localization(at.attackType.name)))
      migPane.add(MediumText(Localization(if (at.ranged) "soldier.ranged" else "soldier.melee")))
      migPane.add(MediumText(s"${at.damage} x ${at.count}"))
      migPane.add(MediumText(at.attributes.map(_.localizedName.localize).mkString(", ")))
    }
    migPane
  }

  val resistancesInfo: Pane = {
    val migPane = new MigPane("wrap 6")
    val attackImages = Map[AttackType, String](Blade -> "sword-human", Impact -> "club", Cold -> "iceball",
      Pierce -> "spear", Fire -> "fireball", Arcane -> "faerie-fire").mapValues(p => AttackView(0, p).attackImage)
    st.resistance.foreach { case (at, c) =>
      migPane.add(new ImageView(attackImages(at)))
      migPane.add(MediumText(Localization(at.name)))
      migPane.add(BigText(c.toString + "%"))
    }
    migPane
  }

  val infoPane = new MigPane("wrap 1") with WorldInterfaceWhiteJavaNode
  infoPane.add(BigText(Localization("soldier.info")), "center")
  infoPane.add(mainInfo)
  infoPane.add(BigText(Localization("soldier.attacks")), "center")
  infoPane.add(attacksInfo)
  infoPane.add(BigText(Localization("soldier.resistances")), "center")
  infoPane.add(resistancesInfo)


  val drPane = new MigPane("wrap 1") with WorldInterfaceWhiteJavaNode
  drPane.add(BigText(Localization("soldier.terrains")), "center")
  drPane.add(defenceAndResistances)

  add(infoPane)
  add(drPane)
}

object SelectRecruitWarriorDialog {

  case class RecruitWarriorOrder(warriorType: WarriorType, warriorCulture: Culture, competence: WarriorCompetence, owner: State, var count: Int) {
    def warrior: Warrior = new Warrior(warriorType, competence, warriorCulture, owner)
  }

}

class SelectRecruitWarriorDialog(owner: Stage, possibleChoices: List[RecruitWarriorOrder]) extends DialogStage[List[RecruitWarriorOrder]] {

  private lazy val choices = possibleChoices.map(_.copy())

  dialogResultProperty.value = Some(choices)

  override protected def dialogContent: Region = {
    val tableView = new TableView[RecruitWarriorOrder]()
    tableView.style = Components.largeFontStyle

    val imageColumn = new TableColumn[RecruitWarriorOrder, ImageView] {
      text = ""
      cellFactory = p => new TableCell[RecruitWarriorOrder, ImageView] {
        override def updateItem(t: ImageView, b: Boolean): Unit = {
          super.updateItem(t, b)
          setGraphic(t)
        }
      }
      cellValueFactory = p => {
        ObjectProperty {
          new ImageView(p.value.warrior.image(1d))
        }
      }
      editable = false
    }

    val name = new StringColumn[RecruitWarriorOrder](Localization("soldier.type"), x =>
      Localization(x.warriorType.name))

    val prof = new StringColumn[RecruitWarriorOrder](Localization("soldier.level"), x =>
      EconomicLocalization.localizeWarriorCompetence(x.competence))

    val info = new TableColumn[RecruitWarriorOrder, Button] {
      text = ""
      cellFactory = p => new TableCell[RecruitWarriorOrder, Button] {
        setAlignment(Pos.CENTER_LEFT)
        override def updateItem(t: Button, b: Boolean): Unit = {
          super.updateItem(t, b)
          setGraphic(t)
        }
      }
      cellValueFactory = p => {
        ObjectProperty {
          new BigButton {
            text = Localization("soldier.info")
            onAction = { _ =>
              new WarriorTypeInfoPane(p.value.warrior).showDialog(owner)
            }
          }
        }
      }
      editable = false
    }

    val counter = new TableColumn[RecruitWarriorOrder, Int] {
      cellValueFactory = p => {
        ObjectProperty(p.value.count)
      }
      cellFactory = p => {
        new TableCell[RecruitWarriorOrder, Int] {
          setAlignment(Pos.CENTER_LEFT)
          val spinner = new Spinner[Int](0, Int.MaxValue, 0, 1)

          spinner.value.onChange { (o, oldValue, newValue) =>
            val order = getTableColumn.getTableView.getItems.get(getTableRow.getIndex)
            order.count = newValue
          }

          override def updateItem(t: Int, b: Boolean): Unit = {
            super.updateItem(t, b)
            setGraphic(spinner)
          }
        }

      }
    }

    tableView.columns ++= List(imageColumn, name, prof, info, counter)
    tableView.items = new ObservableBuffer[RecruitWarriorOrder]() ++ choices
    tableView.prefWidth = 1200
    tableView.prefHeight = 800
    tableView
  }

  override protected def css: Option[String] = Some ("/css/partyPane.css")
}

class ArmyMovementPane(controller: ArmyPaneController, province: Province) extends MigPane with WorldInterfaceJavaNode {
  (province :: province.neighbours).foreach { p =>
    val pane = new WarriorsListTable(p, controller)
    add(pane, "span 1, grow, push")
  }
}

class ArmyPaneController(province: Province, provinceView: ProvinceView, worldState: WorldStateArmyActions) {

  var soldiersWhereChanged: Boolean = false

  val soldiers: Map[Province, ObservableBuffer[Warrior]] = (province :: province.neighbours).map(p => p -> new ObservableBuffer[Warrior]()).toMap
  refreshSoldiersDestinations()

  def regionKey(p: Province): Option[Province] = if (p == province) None else Some(p)

  def refreshSoldiersDestinations(): Unit = {
    soldiers.foreach { case (key, buffer) =>
      val list = province.regionWarriors.warriorDestinations.getOrElse(regionKey(key), Nil)
      buffer.clear()
      buffer ++= list
    }
  }

  var dragContext: Set[Warrior] = Set()

  def performDragAndDrop(target: Province): Unit = {
    soldiersWhereChanged = true
    val to = if (target == province) None else Some(target)
    worldState.planMoveArmy(province, to, dragContext.toList)
    refreshSoldiersDestinations()
    provinceView.refreshSoldiers()
    dragContext = Set()
  }

  def movementPossible(target: Province): Boolean = {
    target == province || worldState.canPlanMoveArmy(province, target, worldState.playerState)
  }

  def dragStartPossible(from: Province): Boolean = {
    val key = if (from == province) None else Some(from)
    province.regionWarriors.warriorDestinations.getOrElse(key, Nil).exists(_.owner == worldState.playerState)
  }

  def disposeSoldier(warrior: Warrior): Unit = {
    soldiersWhereChanged = true
    worldState.disposeSoldier(province, warrior)
    refreshAllSoldiers()
    provinceView.refreshSoldiers()
  }

  val allSoldiers: ObservableBuffer[Warrior] = ObservableBuffer()
  refreshAllSoldiers()

  def refreshAllSoldiers(): Unit = {
    allSoldiers.clear()
    allSoldiers ++= province.regionWarriors.allWarriors
  }

  def canRecruit: Boolean = worldState.playerState == province.owner

  val recruitProjects: ObservableBuffer[BusinessProject] = ObservableBuffer()
  refreshRecruitProjects()

  private def refreshRecruitProjects(): Unit = {
    recruitProjects.clear()
    recruitProjects ++= province.projects.filter(_.isInstanceOf[RecruitWarriorProject])
  }

  def recruitWarrior(warriorType: WarriorType, culture: Culture, competence: WarriorCompetence): Unit = {
    worldState.recruitSoldier(province, competence, warriorType, culture)
    refreshRecruitProjects()
  }

  def possibleWarriorToRecruit(): List[RecruitWarriorOrder] = {
    worldState.possibleWarriorsToRecruit(province).map { case (wt, wc, c) =>
      RecruitWarriorOrder(wt, c, wc, province.owner, 0)
    }.sortBy(ord => (ord.competence.toString, ord.warriorType.name)).reverse
  }
}

class WarriorsListTable(province: Province, controller: ArmyPaneController) extends MigPane with WorldInterfaceWhiteJavaNode {
  val table = new TableView[Warrior] {
    t =>
    rowFactory = { e =>
      val row = new TableRow[Warrior] {
        r =>
        onDragDetected = { e =>
          if (!r.delegate.isEmpty && controller.dragStartPossible(province)) {
            val items = t.delegate.getSelectionModel.getSelectedItems
            controller.dragContext = items.toSet
            val db = r.startDragAndDrop(TransferMode.Move)
            val cc = new ClipboardContent()
            cc.putString(r.index.value.toString)
            db.content = cc
            e.consume()
          }
        }
      }

      row
    }
    style = Components.mediumFontStyle
    onDragOver = { e =>
      if (controller.movementPossible(province)) {
        e.acceptTransferModes(TransferMode.Move)
        e.consume()
      }
    }
    onDragDropped = { e =>
      controller.performDragAndDrop(province)
      e.setDropCompleted(true)
      e.consume()
    }
    disable = !controller.movementPossible(province)
  }

  val soldierImageColumn = new TableColumn[Warrior, Image] {
    text = Localization("army.warrior")
    cellValueFactory = p => ObjectProperty(p.value.image(1d))
    cellFactory = p => new TableCell[Warrior, Image] {
      override def updateItem(t: Image, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(new ImageView(t))
      }
    }
    editable = false
    prefWidth <== table.width * 0.5
  }

  val soldierHP = new TableColumn[Warrior, String] {
    text = Localization("army.hp")
    cellValueFactory = p => ObjectProperty(IntFormatter().format(p.value.hpPercentage * 100) + "%")
    editable = false
    prefWidth <== table.width * 0.45
  }

  table.columns ++= List(soldierImageColumn, soldierHP)
  table.items = controller.soldiers(province)
  table.getSelectionModel.setSelectionMode(SelectionMode.Multiple)
  //GuiUtils.autoFitTable(table)
  add(BigText(province.name).delegate, "center, wrap")
  add(table, "grow, push")
}
