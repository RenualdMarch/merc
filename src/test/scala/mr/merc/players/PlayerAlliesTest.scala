package mr.merc.players

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class PlayerAlliesTest extends FunSuite with BeforeAndAfter {
  val player1 = Player("1")
  val player2 = Player("2")
  val player3 = Player("3")

  before {
    player1.allies = List(player2)
    player2.allies = List(player1)
    player3.allies = Nil
  }

  test("isFriend") {
    assert(player1.isFriend(player2))
    assert(player2.isFriend(player1))
    assert(player1.isFriend(player1))
    assert(player2.isFriend(player2))
    assert(player3.isFriend(player3))
    assert(!player1.isFriend(player3))
    assert(!player2.isFriend(player3))
  }

  test("isSamePlayer") {
    assert(!player1.isSamePlayer(player2))
    assert(!player2.isSamePlayer(player1))
    assert(player1.isSamePlayer(player1))
    assert(player2.isSamePlayer(player2))
    assert(player3.isSamePlayer(player3))
    assert(!player1.isSamePlayer(player3))
    assert(!player2.isSamePlayer(player3))
  }

  test("isEnemy") {
    assert(!player1.isEnemy(player2))
    assert(!player2.isEnemy(player1))
    assert(!player1.isEnemy(player1))
    assert(!player2.isEnemy(player2))
    assert(!player3.isEnemy(player3))
    assert(player1.isEnemy(player3))
    assert(player2.isEnemy(player3))
  }
}