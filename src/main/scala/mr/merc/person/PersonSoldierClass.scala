package mr.merc.person

import mr.merc.unit.SoldierType


import scala.xml.XML

object PersonSoldierClass {
  lazy val configs = readConfig()

  def apply(name: String) = configs(name)

  private def readConfig(): Map[String, PersonSoldierClassInfo] = {
    val xml = XML.load(getClass.getResourceAsStream("/conf/personSoldierTypes.xml"))
    val parsed = (xml \ "personSoldierType").map{node =>
      val name = (node \ "@name").toString()
      val list = (node \ "soldierType").map { st =>
        val name = (st \ "@name").toString()
        val level = (st \ "@level").toString().toInt
        val actualType = SoldierType.apply(name)
        (level, actualType)
      }.toList

      name -> new PersonSoldierClassInfo(list)
    }
    parsed.toMap
  }
}

class PersonSoldierClass(val className: String, info: PersonSoldierClassInfo) {
  def buildSoldierType(level: Int):SoldierType = {
    import info._
    SoldierType(className, cost(level), hp(level), movement(level), 0, level,
      attacks(level), moveCost(level), defence(level),
      resistance(level), attributes(level))
  }
}

class PersonSoldierClassInfo(list:List[(Int, SoldierType)]) {

  private def interpolate(level: Int): Either[((Int, SoldierType), (Int, SoldierType)), (Int, SoldierType)] = {
    list.indexWhere {case (l, _) => l >= level} match {
      case -1 => Left(list.init.last, list.last)
      case 0 => Right(list.head)
      case i if list(i)._1 == level => Right(list(i))
      case i => Left(list(i - 1), list(i))
    }
  }

  private def interpolate(level: Int, f: SoldierType => Int): Int = {
    interpolate(level) match {
      case Right((l, t)) => f(t)
      case Left(((lesserLevel, lesserType), (biggerLevel, biggerType))) =>
        val lesser = f(lesserType)
        val bigger = f(biggerType)
        val y = (level - lesserLevel).toDouble / (biggerLevel - lesserLevel)
        (lesser + y * (bigger - lesser)) toInt
    }
  }

  def hp(level: Int) = interpolate(level, _.hp)

  def movement(level: Int) = interpolate(level, _.movement)

  def cost(level: Int) = interpolate(level, _.cost)

  private def takeFromLower[T](level: Int, f: SoldierType => T): T = {
    interpolate(level) match {
      case Right((_, t)) => f(t)
      case Left(((lesserLevel, lesserType), (biggerLevel, biggerType))) =>
        if (level > biggerLevel) f(biggerType)
        else f(lesserType)
    }
  }

  def attacks(level: Int) = takeFromLower(level, _.attacks)

  def moveCost(level: Int) = takeFromLower(level, _.moveCost)

  def defence(level: Int) = takeFromLower(level, _.defence)

  def resistance(level: Int) = takeFromLower(level, _.resistance)

  def attributes(level: Int) = takeFromLower(level, _.attributes)
}
