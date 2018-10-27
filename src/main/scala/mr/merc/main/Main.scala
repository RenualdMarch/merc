package mr.merc.main

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.Screen
import mr.merc.music.MusicPlayer
import mr.merc.ui.common.SceneManager

object Main extends JFXApp {

  val screenRect = Screen.primary.visualBounds

  stage = new PrimaryStage {
    title = "Mercenary 0.2 (M6)"
    // to make it fit on the screen
    scene = new Scene(screenRect.width - 100, screenRect.height - 100)
    fullScreenExitHint = ""
    fullScreen = false
  }

  val sceneManager = new SceneManager(stage)
  sceneManager.showMainMenu()

  MusicPlayer.playMusic()
}