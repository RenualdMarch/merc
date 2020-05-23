package mr.merc.ui.world

import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.scene.control.TableCell
import mr.merc.economics.WorldGenerationConstants.WorldMapCreationConf
import mr.merc.economics.{FourSeasonsTerrainHexField, WorldGenerator, WorldState}
import mr.merc.local.Localization
import mr.merc.log.Logging
import mr.merc.map.terrain.FourSeasonsTerrainTypes
import mr.merc.politics.State
import mr.merc.ui.common.SceneManager
import mr.merc.ui.dialog.WaitDialog
import mr.merc.ui.minimap.MinimapSize
import mr.merc.ui.world.StateSelectionPane.GeneratedStateData
import org.tbee.javafx.scene.layout.MigPane
import scalafx.application.Platform
import scalafx.beans.property.{BooleanProperty, ObjectProperty, ReadOnlyObjectProperty, StringProperty}
import scalafx.scene.canvas.Canvas
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Spinner, TableColumn, TableView}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.util.{Failure, Success}
import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import mr.merc.util.FxPropertyUtils._
import mr.merc.ui.dialog.ModalDialog._
import mr.merc.unit.view.SoldierView
import mr.merc.util.MercTooltip

class CustomNewGameFrame(sceneManager: SceneManager) extends PaneWithTwoHorizontalChildren(0.6) {

  val minimapPane = new MinimapPane()
  val stateSelectionPane = new StateSelectionPane()
  val configurationPane = new ConfigurationPane(sceneManager, { ws =>
    minimapPane.gameFieldProperty.set(Some(ws.worldHexField))
    stateSelectionPane.applyWorldState(ws)
  }, stateSelectionPane.selectedItem)

  setTwoChildren(
    new MigPane {
      add(configurationPane, "wrap")
      add(stateSelectionPane, "grow, push")
    },
    minimapPane
  )
}

class ConfigurationPane(sceneManager: SceneManager, onWorldCreated: WorldState => Unit, selectedState: ReadOnlyObjectProperty[Option[State]]) extends MigPane() with Logging {
  private val widthLabel = BigText(Localization("customNewGame.width"))
  private val heightLabel = BigText(Localization("customNewGame.height"))
  private val hexesPerProvinceLabel = BigText(Localization("customNewGame.hexesPerProvince"))

  private val worldIsCreating: ObjectProperty[Boolean] = ObjectProperty(false)
  private val generatedWorld: ObjectProperty[Option[WorldState]] = ObjectProperty(None)

  private val widthSpinner = new Spinner[Int](25, Int.MaxValue, 50, 25) {
    style = Components.largeFontStyle
  }

  private val heightSpinner = new Spinner[Int](25, Int.MaxValue, 50, 25) {
    style = Components.largeFontStyle
  }

  private val hexesPerProvinceSpinner = new Spinner[Int](50, Int.MaxValue, 100, 50) {
    style = Components.largeFontStyle
  }

  private val createButton = new BigButton {
    text = Localization("customNewGame.createWorld")
    disable <== worldIsCreating
    onAction = { _ =>
      worldIsCreating.set(true)
      generatedWorld.set(None)
      val waitDialog = new WaitDialog()
      Future {
        val wc = WorldGenerator.generateWorld(config)
        wc.regions.flatMap(_.regionWarriors.allWarriors).foreach { w =>
          new SoldierView(w.soldier, 1d)
        }
        wc
      } onComplete {
        case Success(value) => Platform.runLater {
          onWorldCreated(value)
          generatedWorld.set(Some(value))
          worldIsCreating.set(false)
          waitDialog.close()
        }
        case Failure(exception) => Platform.runLater {
          error(exception.getMessage, exception)
          worldIsCreating.set(false)
          waitDialog.close()

          val alert = new Alert(AlertType.Error) {
            title = Localization("customNewGame.creationError")
            headerText = exception.getMessage
          }
          alert.showAndWait()
        }
      }
      waitDialog.showDialog(sceneManager.stage)
    }
  }

  private val startGameButton = new BigButton {
    text = Localization("customNewGame.startGame")
    disable <== generatedWorld.map(_.isEmpty)
    onAction = { _ =>
      generatedWorld.value.foreach { ws =>
        selectedState.value.foreach { st =>
          ws.playerState = st
          sceneManager.startCustomNewWorld(ws)
        }
      }
    }
  }

  add(widthLabel)
  add(widthSpinner)
  add(heightLabel)
  add(heightSpinner)
  add(hexesPerProvinceLabel)
  add(hexesPerProvinceSpinner, "wrap")
  add(new MigPane {
    add(createButton)
    add(startGameButton)
  }, "span 6, center")

  def config: WorldMapCreationConf = WorldMapCreationConf(
    widthSpinner.getValue,
    heightSpinner.getValue,
    hexesPerProvinceSpinner.getValue
  )
}

class MinimapPane extends Pane {
  private val canvas = new Canvas()

  children.add(canvas)

  canvas.layoutX = 0
  canvas.layoutY = 0
  canvas.width <== this.width
  canvas.height <== this.height

  val gameFieldProperty = ObjectProperty[Option[FourSeasonsTerrainHexField]](None)

  gameFieldProperty.addListener(new ChangeListener[Option[FourSeasonsTerrainHexField]] {
    override def changed(observableValue: ObservableValue[_ <: Option[FourSeasonsTerrainHexField]],
                         t: Option[FourSeasonsTerrainHexField], newValue: Option[FourSeasonsTerrainHexField]): Unit = {
      refreshMapCanvas(newValue)
    }
  })

  this.width.onChange(refreshMapCanvas(gameFieldProperty.value))
  this.height.onChange(refreshMapCanvas(gameFieldProperty.value))

  def refreshMapCanvas(fieldOpt: Option[FourSeasonsTerrainHexField]): Unit = {
    val gc = canvas.graphicsContext2D
    gc.save()
    gc.clearRect(0, 0, width.value, height.value)

    fieldOpt.foreach { field =>
      val minimapSize = MinimapSize(field.width, field.height, width.intValue, height.intValue)
      val side = minimapSize.cellSide
      field.hexes.filterNot(_.terrainMap == FourSeasonsTerrainTypes.FourSeasonsWater).foreach { hex =>
        val x = hex.x * side
        val offset = if (hex.x % 2 == 0) 0 else side / 2
        val y = hex.y * side + offset.ceil
        gc.fill = hex.province.map(_.owner.color).getOrElse(Color.Transparent)
        gc.fillRect(x, y, side, side)
      }
    }
    gc.restore()
  }

  MercTooltip.applyTooltip(canvas, { (x, y) =>
    gameFieldProperty.value.flatMap { field =>
      val minimapSize = MinimapSize(field.width, field.height, width.intValue, height.intValue)
      val side = minimapSize.cellSide

      val xx = x / side

      val offset = if (xx.toInt % 2 == 0) 0 else side / 2
      val yy = (y - offset.ceil) / side

      if (field.isLegalCoords(xx.toInt, yy.toInt)) {
        val h = field.hex(xx.toInt, yy.toInt)
        if (h.terrainMap == FourSeasonsTerrainTypes.FourSeasonsWater) None
        else h.province.map(_.owner.name)
      } else None
    }
  })
}

object StateSelectionPane {

  case class GeneratedStateData(state: State, regionsCount: Int, population: Long, army: Int)

}

class StateSelectionPane extends BorderPane {

  def applyWorldState(ws: WorldState): Unit = {
    val list: List[GeneratedStateData] = ws.states.map { case (st, regions) =>
      GeneratedStateData(st, regions.size,
        regions.flatMap(_.regionPopulation.pops).map(_.populationCount).sum,
        regions.flatMap(_.regionWarriors.allWarriors).size
      )
    }.toList
    statesData.set(list)
  }

  val statesData: ObjectProperty[List[GeneratedStateData]] = ObjectProperty[List[GeneratedStateData]](Nil)
  private val statesDataBuffer = new ObservableBuffer[GeneratedStateData]()
  statesData.onChange {
    statesDataBuffer.clear()
    statesDataBuffer.appendAll(statesData.value.sortBy(-_.population))
  }

  private val tableView = new TableView[GeneratedStateData]()

  tableView.style = Components.mediumFontStyle

  private val stateColumn = new TableColumn[GeneratedStateData, StateComponentColorName] {
    text = Localization("diplomacy.state")
    prefWidth = 350

    cellFactory = p => new TableCell[GeneratedStateData, StateComponentColorName] {
      override def updateItem(t: StateComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }

    cellValueFactory = p => {
      ObjectProperty(new StateComponentColorName(p.value.state))
    }
    editable = false
  }

  private val cultureColumn = new StringColumn[GeneratedStateData](Localization("cultures"),
    x => Localization(x.state.primeCulture.cultureNameKey)) {
    prefWidth = 100
  }

  private val armyColumn = new StringColumn[GeneratedStateData](Localization("army"), _.army.toString)

  private val populationColumn = new StringColumn[GeneratedStateData](Localization("population.populationCount"),
    x => IntFormatter().format(x.population)) {
    prefWidth = 150
  }

  private val regionsColumn = new StringColumn[GeneratedStateData](Localization("regions"),
    _.regionsCount.toString)

  private val partyColumn = new TableColumn[GeneratedStateData, PartyComponentColorName] {
    text = Localization("parliament.rulingParty")
    prefWidth = 350

    cellFactory = p => new TableCell[GeneratedStateData, PartyComponentColorName] {
      override def updateItem(t: PartyComponentColorName, b: Boolean): Unit = {
        super.updateItem(t, b)
        setGraphic(t)
      }
    }
    cellValueFactory = p => {
      ObjectProperty {
        new PartyComponentColorName(p.value.state.politicalSystem.rulingParty)
      }
    }
    editable = false
  }

  tableView.columns ++= List(stateColumn, cultureColumn, partyColumn, regionsColumn, armyColumn, populationColumn)
  tableView.items = statesDataBuffer
  tableView.selectionModel.value.clearSelection()
  center = tableView

  private val selectedItemDelegate: ReadOnlyObjectProperty[GeneratedStateData] = tableView.selectionModel.value.selectedItemProperty()
  val selectedItem: ReadOnlyObjectProperty[Option[State]] = selectedItemDelegate.map(x => Option(x).map(_.state))
}