package mr.merc.local

import scala.io.Source
import java.net.URI
import scala.io.Codec

object Localization {
  private [local] lazy val messages: Map[String, Map[String, String]] = initMessages()

  private val MessageSeparator = "="
  private val ParamHolder = "%s"

  //TODO move to Configuration
  private val defaultLanguage = "ru"
  private [local] var currentLanguage = "ru"
  private val filenamePattern = "/local/messages"
  private val languages = List("en", "ua", "ru")

  def apply(key: String, params: Any*)(implicit language:String = currentLanguage) = getMessage(language, key, params:_*)

  private def getMessage(language:String, key: String, params: Any*) = {
    messages(language).get(key) match {
      case Some(message) => putParamsInMessage(message, params:_*)
      case None => key
    }
  }

  private def putParamsInMessage(message: String, params: Any*) = {
    params.foldLeft(message) ((msg, p) =>
      msg.replaceFirst(ParamHolder, p.toString))
  }

  private def initMessages() = languages.map(l => (l -> parseMessages(l))) toMap

  private def parseMessages(language: String): Map[String, String] = {
    val fileName = filenamePattern + "." + language
    val resource = getClass.getResource(fileName)
    parseLines(resource.toURI)
  }

  private def parseLines(resourceUri: URI) = {
    val lines = Source.fromFile(resourceUri)(Codec("UTF-8")).getLines()
    lines.map(line => {
      val separatorIndex = line.indexOf(MessageSeparator)
      val key = line.take(separatorIndex)
      val message = line.drop(separatorIndex + MessageSeparator.size)
      key-> message
      }).toMap
  }

  
}