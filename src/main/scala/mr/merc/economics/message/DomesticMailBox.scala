package mr.merc.economics.message

import scala.collection.mutable.ArrayBuffer

class DomesticMailBox {
  private val messages:ArrayBuffer[DomesticMessage] = ArrayBuffer()

  def addMessage(message: DomesticMessage): Unit = {
    messages += message
  }
  
  def allMessages:List[DomesticMessage] = messages.toList

  def clearMessages(): Unit = messages.clear()

}
