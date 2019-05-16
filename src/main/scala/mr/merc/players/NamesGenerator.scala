package mr.merc.players

import mr.merc.economics.Culture.CultureInfo

import scala.util.Random

class NamesGenerator(cultureInfo: CultureInfo) {
  val cityNames = new RandomCyclicStream[String](cultureInfo.cities)
  val stateNames = new RandomCyclicStream[String](cultureInfo.states)
}

class RandomCyclicStream[T](list:List[T]) {
  private var stream = Stream.continually(Random.shuffle(list)).flatten

  def extract(): T = {
    val r = stream.head
    stream = stream.drop(1)
    r
  }
}
