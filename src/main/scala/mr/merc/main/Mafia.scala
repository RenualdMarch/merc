package mr.merc.main

import mr.merc.economics.MapUtil.FloatOperations._

object Mafia {
  val PeopleCount = 12
  val MafiaCount = 4

  case class Position(player:Int, red:Set[Int], black:Set[Int]) {
    val actualPosition:Map[Int, Double] = {
      val blacks = black.map(_ -> 1d).toMap
      val reds = red.map(_ -> 0d).toMap
      val remSum = MafiaCount - black.size
      val remPeople = PeopleCount - black.size - red.size
      val remMap = ((1 to PeopleCount).toSet -- red -- black).map (_ -> remSum.toDouble / remPeople).toMap
      blacks |+| reds |+| remMap
    }

    def alignQ(distribution: TeamDistribution): Double = {
      if (red.size + black.size == 0) 0
      else if (distribution.black.contains(player)) {
        -distribution.black.map { b =>
          actualPosition.getOrElse(b, 0d)
        }.sum
      } else if (distribution.red.contains(player)) {
        0
      } else {
        sys.error(s"Player $player is not black nor red in $distribution")
      }
    }
  }

  case class TeamDistribution(red:Set[Int], black:Set[Int]) {
    def distributionQ(positions:List[Position]):Double = {
      positions.map(_.alignQ(this)).sum
    }
  }

  val positions:List[Position] = List(
    Position(1, red = Set(), black = Set(5)),
    Position(2, red = Set(), black = Set(1,5)),
    Position(3, red = Set(), black = Set(6,11,12,8)),
    Position(4, red = Set(5,2), black = Set(1,3)),
    Position(5, red = Set(10), black = Set(1,2,3,4)),
    Position(6, red = Set(2,5), black = Set(1,3)),
    Position(7, red = Set(2,3,4,8), black = Set(1,10,11)),
    Position(8, red = Set(6,11,12,5), black = Set(3,4)),
    Position(9, red = Set(5,6), black = Set(1)),
    Position(10, red = Set(5,6,9,10,8), black = Set(7)),
    Position(11, red = Set(1,8), black = Set(2,3,5)),
    Position(12, red = Set(10,9,8,5,3), black = Set(11)),
  )


  def main(args: Array[String]): Unit = {
    val max = combinations().map(c => c -> c.distributionQ(positions)).sortBy(-_._2)
    val size = max.size
    max.take(50).foreach { case (d, q) =>
      println(d, q)
    }

    val r = max.zipWithIndex.map { case ((d, q), i) =>
      d.black.map(_ -> (size - i) / size.toDouble).toMap
    }.reduce(_ |+| _).toList.sortBy(-_._2)
    r.foreach { case (i, q) =>
      println(i, q)
    }
  }

  private def combinations():List[TeamDistribution] = {
    val fullSet = (1 to PeopleCount).toSet
    fullSet.subsets(MafiaCount).map { black =>
      TeamDistribution(fullSet -- black, black)
    }.toList
  }
}
