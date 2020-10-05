package mr.merc.ui.world

import java.io.File

import javafx.event.EventHandler
import mr.merc.economics.{Battle, SeasonOfYear, WorldGenerator, WorldState}
import mr.merc.log.Logging
import mr.merc.map.hex.view.ProvinceView
import mr.merc.map.hex.view.TerrainHexFieldView.WorldMapViewMode
import mr.merc.map.view.MapView
import mr.merc.politics.{Province, State}
import mr.merc.ui.common.{CanvasLayers, SceneManager}
import scalafx.geometry.Rectangle2D
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.Includes._
import javafx.scene.input.{KeyEvent => JKeyEvent}
import javafx.scene.input.{KeyCode => JKeyCode}
import mr.merc.ai.BattleAI
import mr.merc.economics.Seasons.Season
import mr.merc.game.{GameContainer, SaveLoad}
import mr.merc.local.Localization
import mr.merc.map.hex.TerrainHexField
import mr.merc.ui.battle.BattleFrame
import mr.merc.ui.dialog.WaitDialog
import org.tbee.javafx.scene.layout.MigPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import mr.merc.ui.dialog.ModalDialog._
import scalafx.application.Platform
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.Alert
import scalafx.scene.control.Alert.AlertType
import scalafx.stage.FileChooser

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object WorldFrame {
  val PixelsPerScroll = 200
}

class WorldFrame(val sceneManager: SceneManager, worldState: WorldState) extends Pane with Logging {
  val factor = 1d

  private val dateProperty = ObjectProperty(worldState.seasonOfYear)

  private var prevCurrentMap:(Season, TerrainHexField) =
    (worldState.seasonOfYear.season, worldState.worldHexField.buildTerrainHexField(worldState.seasonOfYear.season))
  def currentMap:TerrainHexField = {
    if (worldState.seasonOfYear.season != prevCurrentMap._1) {
      prevCurrentMap = (worldState.seasonOfYear.season, worldState.worldHexField.buildTerrainHexField(worldState.seasonOfYear.season))
    }
    prevCurrentMap._2
  }

  private var mapView = initMapView()
  private var provinceViewsMap = initProvinceViews()

  def initMapView(): MapView = new MapView(currentMap, factor, mode = WorldMapViewMode)

  def initProvinceViews(): Map[Province, ProvinceView] = worldState.states.flatMap { case (_, pList) =>
    pList.map { p =>
      new ProvinceView(worldState.seasonOfYear.season, p, worldState.worldHexField, currentMap, mapView.terrainView)
    }
  }.map(p => p.province -> p).toMap

  val menu = new WorldMenu(this)
  val stateLabel: Pane = new PlayerStateData(worldState.playerState)
  val dateLabel: Pane = new TurnData(dateProperty)

  private val worldCanvas = new CanvasLayers(mapView.canvasBattleLayers, new Rectangle2D(0, 0, mapView.pixelWidth, mapView.pixelHeight))
  private val interfacePane = new WorldInterfacePane(this, worldCanvas, currentMap, factor, mapView.pixelWidth, mapView.pixelHeight)

  menu.prefWidth <== this.width - this.stateLabel.width - this.dateLabel.width
  menu.layoutX <== this.stateLabel.width
  stateLabel.prefHeight <== menu.height
  dateLabel.prefHeight <== menu.height
  dateLabel.layoutX <== this.width - this.dateLabel.width
  interfacePane.layoutY <== menu.height
  interfacePane.prefWidth <== this.width
  interfacePane.prefHeight <== this.height - menu.height

  this.children = List(stateLabel, menu, dateLabel, interfacePane)

  worldCanvas.onMouseClicked = (event: MouseEvent) => {
    info(s"clicked on (${event.x}, ${event.y})")
    val rect = worldCanvas.viewRect
    val provinceOpt = mapView.provinceByPixel(event.x + rect.minX toInt, event.y + rect.minY toInt)
    provinceOpt.foreach { p =>
      info(s"selected province ${p.name}")
      val pane = new InterfacePane(new ProvinceDetailsPane(p, this), () => {
        interfacePane.removeRightTopPanel()
        interfacePane.removeFacePanel()
      })
      interfacePane.setRightTopPanel(pane)
    }
  }

  totalRefresh()

  def totalRefresh(): Unit = {
    hideFullPane()
    hideFacePane()
    this.mapView = initMapView()
    this.provinceViewsMap = initProvinceViews()
    this.provinceViewsMap.foreach { case (_, pv) =>
      pv.refreshCity()
      pv.refreshSoldiers()
    }
    worldCanvas.layers = mapView.canvasBattleLayers
    worldCanvas.redraw()
    interfacePane.refreshMinimap(currentMap)
    dateProperty.value = worldState.seasonOfYear
  }

  def showPopulationPane(province: Province) {
    val pane = new PopulationViewPane(province)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showEnterprisesPane(province: Province): Unit = {
    val pane = new EnterprisesViewPane(province, sceneManager.stage, worldState)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showArmyMovement(province: Province): Unit = {
    val pane = new ArmyPane(province, provinceViewsMap(province), worldState, sceneManager.stage)
    interfacePane.setFullPanel(new InterfacePane(pane, () => {
      if (pane.soldiersWhereChanged) {
        worldCanvas.redraw()
      }
      hideFullPane()
    }))
  }

  def showMarket(province: Province): Unit = {
    val pane = new MarketViewPane(province, sceneManager.stage)
    interfacePane.setFacePanel(new InterfacePane(pane, () => hideFacePane()))
  }

  def showBudgetPane(): Unit = {
    val pane = new BudgetPane(worldState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def showForeignTradePane(): Unit = {
    val pane = new StateTradePane(worldState.stateProduction, worldState.playerState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def showParliamentPane(): Unit = {
    val pane = new ParliamentPane(sceneManager, worldState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def showDiplomacyPane(): Unit = {
    val pane = new DiplomacyPane(worldState, worldState.playerState, sceneManager.stage, this)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def showMailPane(): Unit = {
    val pane = new MailPane(worldState.playerState, worldState)
    interfacePane.setFullPanel(new InterfacePane(pane, () => hideFullPane()))
  }

  def hideFacePane(): Unit = {
    interfacePane.removeFacePanel()
  }

  def hideFullPane(): Unit = {
    interfacePane.removeFullPanel()
  }

  def showMinimap(): Unit = {
    interfacePane.showMinimap()
  }

  def hideMinimap(): Unit = {
    interfacePane.hideMinimap()
  }

  def nextTurn(): Unit = {
    val waitDialog = new WaitDialog

    Future(worldState.nextTurn(true)).map { battles =>
      Platform.runLater {
        Try {
          if (battles.nonEmpty) {
            playBattles(battles, () => {
              worldState.sendBattleReports()
              totalRefresh()
              sceneManager.showFrame(this)
              showMailPane()
            })
          } else {
            worldState.sendBattleReports()
            totalRefresh()
            sceneManager.showFrame(this)
            showMailPane()
          }
        } match {
          case Success(()) =>
            waitDialog.close()
          case Failure(ex) =>
            waitDialog.close()
            error(ex.getMessage, ex)
        }
      }
    }

    waitDialog.showDialog(sceneManager.stage)
  }

  this.addEventFilter(javafx.scene.input.KeyEvent.KEY_PRESSED, new EventHandler[JKeyEvent] {
    override def handle(t: JKeyEvent): Unit = {
      if (Set(JKeyCode.UP, JKeyCode.DOWN, JKeyCode.LEFT, JKeyCode.RIGHT).contains(t.getCode)) {
        keyPressed(t)
        t.consume()
      }
    }
  })

  this.onScroll = { event =>
    val delta = event.getDeltaY
    val diff = delta * 1.75
    val d = diff / mapView.pixelHeight
    val v = keepHValueInBounds(worldCanvas.vvalue.value - d)
    worldCanvas.vvalue.value = v
  }

  private def keepHValueInBounds(hv: Double): Double = {
    if (hv < 0) 0
    else if (hv > 1) 1
    else hv
  }

  private def keepWValueInBounds(wv: Double): Double = {
    if (wv < 0) 0
    else if (wv > 1) 1
    else wv
  }

  private def keyPressed(event: KeyEvent): Unit = {
    info(s"Pressed ${event.code}")
    event.code match {
      case KeyCode.Up =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelHeight
        val v = keepHValueInBounds(worldCanvas.vvalue.value - d)
        worldCanvas.vvalue.value = v
      case KeyCode.Down =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelHeight
        val v = keepHValueInBounds(worldCanvas.vvalue.value + d)
        worldCanvas.vvalue.value = v
      case KeyCode.Left =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelWidth
        val v = keepWValueInBounds(worldCanvas.hvalue.value - d)
        worldCanvas.hvalue.value = v
      case KeyCode.Right =>
        val d = WorldFrame.PixelsPerScroll.toDouble / mapView.pixelWidth
        val v = keepWValueInBounds(worldCanvas.hvalue.value + d)
        worldCanvas.hvalue.value = v
      case _ => // do nothing
    }
  }

  def playBattles(battles: List[Battle], callback:() => Unit): Unit = {
    def callbackFunction(remainingBattles: List[Battle])(): Unit = {
      remainingBattles match {
        case Nil =>
          battles.foreach(worldState.concludePlayerBattle)
          callback()
        case some :: rem =>
          if (some.isOver) {
            callbackFunction(rem)
          } else {
            val ai = some.gameField.players.map(_ -> BattleAI()).toMap - worldState.playerState.toPlayer
            val battleFrame = new BattleFrame(sceneManager, some.gameField, ai, callbackFunction(rem))
            sceneManager.showFrame(battleFrame)
          }
      }
    }

    callbackFunction(battles)()
  }

  def saveGame(): Unit = {
    val chooser = new FileChooser()
    chooser.setInitialDirectory(SaveLoad.saveDirectory())
    Option(chooser.showSaveDialog(sceneManager.stage)).foreach { f =>
      SaveLoad.save(GameContainer(worldState), f) match {
        case Success(_) =>
          val alert = new Alert(AlertType.Information)
          alert.setTitle(Localization("saveDialog.title"))
          alert.setHeaderText(null)
          alert.setContentText(Localization("saveDialog.content"))
          info(s"Game saved to ${f.getAbsolutePath}")
          alert.showAndWait()
        case Failure(exception) =>
          val alert = new Alert(AlertType.Error)
          alert.setTitle(Localization("saveDialogFailed.title"))
          alert.setHeaderText(Localization("saveDialogFailed.content"))
          alert.setContentText(exception.getMessage)
          error(s"Failed to save game to ${f.getAbsolutePath}")
          error(exception.getMessage, exception)
          alert.showAndWait()
      }
    }
  }


  class PlayerStateData(state: State) extends MigPane("center") with WorldInterfaceJavaNode {
    val rect = Rectangle(Components.largeFontSize, Components.largeFontSize)
    rect.fill = state.color
    rect.stroke = Color.Black
    val text = BigText(state.name)
    add(rect)
    add(text)
  }

  class TurnData(property: ObjectProperty[SeasonOfYear]) extends MigPane("center") with WorldInterfaceJavaNode {

    import mr.merc.util.FxPropertyUtils._

    val date = new BigText {
      text <== property.map(_.localizedString)
    }
    add(date)
  }

}


