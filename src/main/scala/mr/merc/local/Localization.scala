package mr.merc.local

import scala.io.Source
import java.net.URI
import scala.io.Codec
import mr.merc.conf.Conf
import java.io.InputStream

object Localization {
  private[local] lazy val messages: Map[String, Map[String, String]] = initMessages()

  def languages = List("en", "ua", "ru")

  private val MessageSeparator = "="
  private val ParamHolder = "%s"

  //TODO take from configuration
  private def currentLanguage = Conf.string("Language")
  private val filenamePattern = "/local/messages"
  def apply(key: String, params: Any*) = getMessage(currentLanguage, key, params: _*)

  private def getMessage(language: String, key: String, params: Any*) = {
    messages(language).get(key) match {
      case Some(message) => putParamsInMessage(message, params: _*)
      case None => key
    }
  }

  private def putParamsInMessage(message: String, params: Any*) = {
    params.foldLeft(message)((msg, p) =>
      msg.replaceFirst(ParamHolder, p.toString))
  }

  private def initMessages() = languages.view.map(l => (l -> parseMessages(l))) toMap

  private def parseMessages(language: String): Map[String, String] = {
    val fileName = filenamePattern + "." + language
    parseLines(getClass.getResourceAsStream(fileName))
  }

  private def parseLines(stream: InputStream) = {
    val lines = Source.fromInputStream(stream)(Codec("UTF-8")).getLines()
    lines.map(line => {
      val separatorIndex = line.indexOf(MessageSeparator)
      val key = line.take(separatorIndex)
      val message = line.drop(separatorIndex + MessageSeparator.size)
      key -> message
    }).toMap
  }

}