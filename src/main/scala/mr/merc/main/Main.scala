package mr.merc.main

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.beans.property.DoubleProperty.sfxDoubleProperty2jfx
import scalafx.scene.Group.sfxGroup2jfx
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Stop.sfxStop2jfx
import scalafx.scene.paint.Color
import scalafx.scene.paint.CycleMethod
import scalafx.scene.paint.LinearGradient
import scalafx.scene.paint.Stop
import scalafx.scene.shape.Rectangle
import scalafx.scene.Group
import scalafx.scene.Scene
import scalafx.stage.Stage
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Screen
import mr.merc.map.hex.view.TerrainHexView
import mr.merc.map.hex.TerrainHex
import mr.merc.map.terrain.Grass
import mr.merc.map.hex.TerrainHexField
import mr.merc.map.hex.view.TerrainHexFieldView
import scalafx.scene.image.Image
import mr.merc.map.terrain.Sand
import mr.merc.map.terrain.Water
import mr.merc.map.terrain.Swamp
import mr.merc.map.terrain.Road
import mr.merc.map.terrain.Forest
import mr.merc.map.objects.WoodenBridge
import mr.merc.map.objects.House
import mr.merc.map.terrain.Mountain
import mr.merc.map.view.MapView
import mr.merc.unit.Soldier
import mr.merc.unit.SoldierType
import scalafx.animation.Timeline
import scalafx.animation.KeyFrame
import scalafx.util.Duration
import scalafx.animation.Animation
import mr.merc.players.Player
import javafx.{ fxml => jfxf }
import javafx.{ scene => jfxs }
import java.util.ResourceBundle
import java.io.FileInputStream
import java.util.PropertyResourceBundle
import mr.merc.local.MercResourceBundle
import mr.merc.ui.battle.BattleFrame
import mr.merc.music.MusicPlayer
import mr.merc.ui.menu.MainMenu
import mr.merc.ui.common.SceneManager
import scalafx.scene.shape.Polygon

object Main extends JFXApp {

  val screenRect = Screen.primary.visualBounds

  stage = new PrimaryStage {
    title = "Mercenary 0.1 (M4)"
    scene = new Scene(screenRect.width - 100, screenRect.height - 100) // to make it fit on the screen
    // TODO check on java 8
    // fullScreen = true
  }
  val sceneManager = new SceneManager(stage)
  sceneManager.showMainMenu()

  MusicPlayer.playMusic()
}