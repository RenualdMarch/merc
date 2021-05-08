package mr.merc.util

class RecalculatableValue[T](f: => T) {

  private var innerValue: Option[T] = None
  
  def value: T = innerValue.getOrElse {
    val v = f
    innerValue = Some(v)
    v
  }
  
  def clear(): Unit = {
    innerValue = None
  }
}
