package mr.merc.conf

import java.util.Properties
import java.io.FileReader
import java.io.File
import java.io.FileOutputStream
import collection.JavaConversions._
import scala.concurrent._
import java.util.concurrent.Executors
import java.util.Locale
import mr.merc.local.Localization

object Conf {
  private implicit val executor = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
  val confPath = "merc.properties"
  val defaultConf = Map("Sound" -> "true", "Music" -> "false", "DevMode" -> "true") ++ localizationProps

  private def localizationProps: Map[String, String] = {
    val currentLanguage = Locale.getDefault().getLanguage()
    if (Localization.languages.contains(currentLanguage)) {
      Map("Language" -> currentLanguage)
    } else {
      Map("Language" -> "en")
    }

  }

  private var conf: Conf = readProperties
  def string(name: String) = conf.properties(name)
  def bool(name: String) = string(name: String).toBoolean

  private[conf] def readProperties: Conf = {
    val file = new File(confPath)
    val values = if (file.exists()) {
      val props = new Properties
      val reader = new FileReader(file)
      try {
        props.load(reader)
      } finally {
        reader.close()
      }
      props toMap
    } else {
      Map[String, String]()
    }

    new Conf(defaultConf ++ values)
  }

  def writeChanges(changes: Map[String, String], applyChanges: Boolean, writeToFile: Boolean = true) {
    if (applyChanges) {
      conf = new Conf(conf.properties ++ changes)
    }

    if (writeToFile) {
      Future {
        conf = new Conf(readProperties.properties ++ changes)
        val props = new Properties
        conf.properties.foreach { case (k, v) => props.put(k, v) }
        val out = new FileOutputStream(confPath)
        try {
          props.store(out, null)
        } finally {
          out.close()
        }
      }
    }

    // TODO add logging on success and failure
  }
}

private class Conf(val properties: Map[String, String]) {
  def string(name: String) = properties(name)
  def bool(name: String) = string(name: String).toBoolean
}