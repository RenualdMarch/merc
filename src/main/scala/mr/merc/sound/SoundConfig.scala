package mr.merc.sound

import xml.XML
import java.io.File

object SoundConfig {
  private val pathToSounds = "/sounds/"

  val soundsMap = loadSounds

  private def loadSounds: Map[String, Sound] = {
    val path = getClass.getResource("/conf/sounds.xml").toURI
    val xml = XML.loadFile(new File(path))

    val sounds = for (node <- xml \ "sound") yield {
      val name = (node \ "@name").toString()
      val path = pathToSounds + (node \ "@path").toString()
      (name, new Sound(path))
    }

    sounds.toMap
  }
}