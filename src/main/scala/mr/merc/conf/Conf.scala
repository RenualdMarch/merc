package mr.merc.conf

import java.util.Properties
import java.io.FileReader
import java.io.File
import java.io.FileOutputStream
import collection.JavaConversions._
import scala.concurrent._
import ExecutionContext.Implicits.global

object Conf {
    val confPath = "merc.properties"
	val defaultConf = Map("Sound" -> "true", "Music" -> "true", "devMode" -> "true")
	
	def readProperties:Conf = {
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
    
    def writeChanges(changes:Map[String, String]) {
      val f = future {
        val currentConf = readProperties
        val result = currentConf.properties ++ changes
        val props = new Properties
        result foreach {case (k,v) => props.put(k, v)}
        val out = new FileOutputStream(confPath)
        try {
          props.store(out, null)
        } finally {
          out.close()
        }
      }
      
      // TODO add logging on success and failure
    }
}

class Conf(val properties:Map[String, String]) {
  def string(name:String) = properties(name)
  def bool(name:String) = string(name:String).toBoolean 
}