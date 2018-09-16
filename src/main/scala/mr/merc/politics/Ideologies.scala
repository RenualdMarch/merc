package mr.merc.politics

object Ideologies {

  sealed abstract class Ideology

  // monarchy, expansion, protectionism, religion
  case object Monarchists extends Ideology

  // monarchy, expansion, protectionism, no religion
  case object Nationalists extends Ideology

  // magocracy, expansion, protectionism, religion
  case object Magism extends Ideology

  // constitutional monarchy, expansion, protectionism, religion
  case object Conservative extends Ideology

  // democracy, pacifist, free trade, freedom of religion, economical freedom
  case object Liberal extends Ideology

  // democracy, pacifist, free trade, no religion, high corporate taxes
  case object Socialist extends Ideology
}
