package mr.merc.game

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}
import com.twitter.chill.ScalaKryoInstantiator
import mr.merc.economics.WorldState

import scala.util.Try

object SaveLoad {

  private val inst = new ScalaKryoInstantiator()
  private val kryo = inst.newKryo()

  kryo.register(classOf[GameContainer]);

  def save(container: GameContainer, file: File): Try[Unit] = Try {
    val oos = new Output(new FileOutputStream(file))
    try {
      kryo.writeObject(oos, container)
    } finally {
      oos.close()
    }
  }

  def load(file: File): Try[GameContainer] = Try {
    val ois = new Input(new FileInputStream(file))
    try {
      kryo.readObject(ois, classOf[GameContainer])
    } finally {
      ois.close()
    }
  }

  def saveDirectory():File = {
    val userHome = System.getProperty("user.home")
    val saveDir = new File(s"$userHome/merc")
    if (!saveDir.exists()) {
      saveDir.mkdir()
    }
    saveDir
  }
}

case class GameContainer(state: WorldState)