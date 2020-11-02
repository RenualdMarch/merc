package mr.merc.sound

import xml.XML
import java.io.File

object SoundConfig {
  private val pathToSounds = "/sounds/"

  val soundsMap = loadSounds

  private def loadSounds: Map[String, Sound] = {
    val xml = XML.load(getClass.getResourceAsStream("/conf/sounds.xml"))

    val sounds = for (node <- xml \ "sound") yield {
      val name = (node \ "@name").toString()
      val path = pathToSounds + (node \ "@path").toString()
      (name, new Sound(path))
    }

    sounds.toMap
  }
}