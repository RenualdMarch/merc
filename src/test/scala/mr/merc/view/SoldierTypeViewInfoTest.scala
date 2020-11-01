package mr.merc.view

import org.scalatest.FunSuite
import mr.merc.unit.view.SoldierViewAttackState
import mr.merc.map.hex._
import mr.merc.unit.view.DeathState
import mr.merc.unit.view.DefenceState
import mr.merc.unit.view.IdleState
import mr.merc.unit.view.MoveState
import mr.merc.unit.view.StandState
import mr.merc.image.LazyMirroredVerticallyImage
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.unit.sound._
import org.scalatest.Matchers

class SoldierTypeViewInfoTest extends FunSuite with Matchers {

  test("loading without optional parameters") {
    val vt = SoldierTypeViewInfo("testType1")
    val attack1ns = vt.images(SoldierViewAttackState(true, N, 0))
    assert(attack1ns.size === 2)
    assert(attack1ns(0).imagePath.get === "/images/units/testType1/im1n.png")
    assert(attack1ns(0).alpha === 0.5)
    assert(attack1ns(0).xOffset === 5)
    assert(attack1ns(0).yOffset === 3)
    assert(attack1ns(1).imagePath.get === "/images/units/testType1/im2n.png")
    assert(attack1ns(1).alpha === 1)
    assert(attack1ns(1).xOffset === 0)
    assert(attack1ns(1).yOffset === 0)

    val attack1nf = vt.images(SoldierViewAttackState(false, N, 0))
    assert(attack1nf.size === 1)
    assert(attack1nf(0).imagePath.get === "/images/units/testType1/im3n.png")

    val attack1nes = vt.images(SoldierViewAttackState(true, NE, 0))
    assert(attack1nes.size === 2)
    assert(attack1nes(0).imagePath.get === "/images/units/testType1/im1m.png")
    assert(attack1nes(1).imagePath.get === "/images/units/testType1/im2m.png")

    val attack1nef = vt.images(SoldierViewAttackState(false, NE, 0))
    assert(attack1nef.size === 1)
    assert(attack1nef(0).imagePath.get === "/images/units/testType1/im3m.png")

    val attack1ses = vt.images(SoldierViewAttackState(true, SE, 0))
    assert(attack1ses.size === 2)
    assert(attack1ses(0).imagePath.get === "/images/units/testType1/im1r.png")
    assert(attack1ses(1).imagePath.get === "/images/units/testType1/im2r.png")

    val attack1sef = vt.images(SoldierViewAttackState(false, SE, 0))
    assert(attack1sef.size === 1)
    assert(attack1sef(0).imagePath.get === "/images/units/testType1/im3r.png")

    val attack1ss = vt.images(SoldierViewAttackState(true, S, 0))
    assert(attack1ss.size === 2)
    assert(attack1ss(0).imagePath.get === "/images/units/testType1/im1s.png")
    assert(attack1ss(1).imagePath.get === "/images/units/testType1/im2s.png")

    val attack1sf = vt.images(SoldierViewAttackState(false, S, 0))
    assert(attack1sf.size === 2)
    assert(attack1sf(0).imagePath.get === "/images/units/testType1/im1s.png")
    assert(attack1sf(1).imagePath.get === "/images/units/testType1/im2s.png")

    val attack1sws = vt.images(SoldierViewAttackState(true, SW, 0))
    assert(attack1sws.size === 2)
    assert(attack1sws(0).imagePath.get === "/images/units/testType1/im1r.png")
    assert(attack1sws(1).imagePath.get === "/images/units/testType1/im2r.png")

    val attack1swf = vt.images(SoldierViewAttackState(false, SW, 0))
    assert(attack1swf.size === 1)
    assert(attack1swf(0).imagePath.get === "/images/units/testType1/im3r.png")

    val attack1nws = vt.images(SoldierViewAttackState(true, NW, 0))
    assert(attack1nws.size === 2)
    assert(attack1nws(0).imagePath.get === "/images/units/testType1/im1m.png")
    assert(attack1nws(1).imagePath.get === "/images/units/testType1/im2m.png")

    val attack1nwf = vt.images(SoldierViewAttackState(false, NW, 0))
    assert(attack1nwf.size === 1)
    assert(attack1nwf(0).imagePath.get === "/images/units/testType1/im3m.png")

    for (d <- Direction.list; b <- List(true, false); n = 1) {
      val s = SoldierViewAttackState(b, d, n)
      val attack2 = vt.images(s)

      if (s.success) {
        assert(attack2.size === 2)
        assert(attack2(0).imagePath.get === "/images/units/testType1/im4a.png")
        assert(attack2(1).imagePath.get === "/images/units/testType1/im5a.png")
      } else {
        assert(attack2.size === 1)
        assert(attack2(0).imagePath.get === "/images/units/testType1/im6a.png")
      }
    }

    val idle = vt.images(IdleState)
    assert(idle.size === 2)
    assert(idle(0).imagePath.get === "/images/units/testType1/im7.png")
    assert(idle(1).imagePath.get === "/images/units/testType1/im8.png")

    val stand = vt.images(StandState)
    assert(stand.size === 1)
    assert(stand(0).imagePath.get === "/images/units/testType1/im9.png")

    val death = vt.images(DeathState)
    assert(death.size === 2)
    assert(death(0).imagePath.get === "/images/units/testType1/im10.png")
    death(0).alpha should be(0.7f +- 0.01f)
    assert(death(1).imagePath.get === "/images/units/testType1/im11.png")
    death(1).alpha should be(0.3f +- 0.01f)

    val move = vt.images(MoveState)
    assert(move.size === 1)
    assert(move(0).imagePath.get === "/images/units/testType1/im12.png")

    val defence = vt.images(DefenceState)
    assert(defence.size === 1)
    assert(defence(0).imagePath.get === "/images/units/testType1/im13.png")
  }

  test("loading using only required parameters") {
    val vt = SoldierTypeViewInfo("testType2")
    for (d <- Direction.list; b <- List(true, false); n <- List(0, 1, 2)) {
      val s = SoldierViewAttackState(b, d, n)
      val attack = vt.images(s)
      val configurable = Set(N, NE, SE, S)

      assert(attack.size === 1)
      if (n == 0 || n == 1) {
        assert(attack(0).imagePath.get === "/images/units/testType2/im14.png")
      } else if (n == 2) {
        assert(attack(0).imagePath.get === "/images/units/testType2/im15.png")
      }
      assert(attack(0).alpha === 1)
      assert(attack(0).xOffset === 0)
      assert(attack(0).yOffset === 0)
    }

    val idle = vt.images(IdleState)
    assert(idle.size === 1)
    assert(idle(0).imagePath.get === "/images/units/testType2/im15.png")

    val move = vt.images(MoveState)
    assert(move.size === 1)
    assert(move(0).imagePath.get === "/images/units/testType2/im15.png")

    val defence = vt.images(DefenceState)
    assert(defence.size === 1)
    assert(defence(0).imagePath.get === "/images/units/testType2/im15.png")

    val stand = vt.images(StandState)
    assert(stand.size === 1)
    assert(stand(0).imagePath.get === "/images/units/testType2/im15.png")

    val death = vt.images(DeathState)
    assert(death.size === 4)
    assert(death(0).imagePath.get === "/images/units/testType2/im15.png")
    death(0).alpha should be(0.8f +- 0.01f)
    assert(death(1).imagePath.get === "/images/units/testType2/im15.png")
    death(1).alpha should be(0.6f +- 0.01f)
    assert(death(2).imagePath.get === "/images/units/testType2/im15.png")
    death(2).alpha should be(0.4f +- 0.01f)
    assert(death(3).imagePath.get === "/images/units/testType2/im15.png")
    death(3).alpha should be(0.2f +- 0.01f)
  }

  test("sounds") {
    val sounds = SoldierTypeViewInfo("testType1").sounds
    assert(sounds(MovementSound).path === "/sounds/5.mp3")
    assert(sounds(PainSound).path === "/sounds/7.mp3")
    assert(sounds(DeathSound).path === "/sounds/6.mp3")
    assert(sounds(AttackSound(0, true)).path === "/sounds/1.mp3")
    assert(sounds(AttackSound(0, false)).path === "/sounds/2.mp3")
    assert(sounds(AttackSound(1, true)).path === "/sounds/3.mp3")
    assert(sounds(AttackSound(1, false)).path === "/sounds/4.mp3")
  }

  test("absent sounds") {
    val sounds = SoldierTypeViewInfo("testType2").sounds
    assert(sounds(MovementSound).path === "/sounds/50.mp3")
    assert(sounds.get(PainSound) === None)
    assert(sounds.get(DeathSound) === None)
    assert(sounds(AttackSound(0, true)).path === "/sounds/10.mp3")
    assert(sounds(AttackSound(0, false)).path === "/sounds/20.mp3")
    assert(sounds.get(AttackSound(1, true)) === None)
    assert(sounds.get(AttackSound(1, false)) === None)
  }

  test("attack views") {
    val attacks1 = SoldierTypeViewInfo("testType1").attacks
    assert(attacks1.size === 2)
    assert(attacks1(0).index === 0)
    assert(attacks1(0).imageName === "image1")
    assert(attacks1(0).projectile === Some("testProjectile3"))
    assert(attacks1(1).index === 1)
    assert(attacks1(1).imageName === "image10")
    assert(attacks1(1).projectile === None)
  }

  test("loading one image soldier") {
    val vt = SoldierTypeViewInfo("oneImageSoldier")
    val attacks = vt.attacks
    attacks should have size 2
    val List(at1, at2) = attacks
    at1.index shouldBe 0
    at1.projectile shouldBe None
    at1.imageName shouldBe "111"
    at2.index shouldBe 1
    at2.projectile shouldBe Some("testProjectile3")
    at2.imageName shouldBe "222"

    for(d <- Direction.list; b <- List(true, false); n <- List(0, 1)) {
      val images = vt.images(SoldierViewAttackState(b, d, n))
      images should have size 1
      images.head.imagePath shouldBe Some("/images/units/oneImageSoldier/im14.png")
    }

    val idle = vt.images(IdleState)
    assert(idle.size === 1)
    assert(idle.head.imagePath.get === "/images/units/oneImageSoldier/im14.png")

    val move = vt.images(MoveState)
    assert(move.size === 1)
    assert(move.head.imagePath.get === "/images/units/oneImageSoldier/im14.png")

    val defence = vt.images(DefenceState)
    assert(defence.size === 1)
    assert(defence.head.imagePath.get === "/images/units/oneImageSoldier/im14.png")

    val stand = vt.images(StandState)
    assert(stand.size === 1)
    assert(stand.head.imagePath.get === "/images/units/oneImageSoldier/im14.png")

    val death = vt.images(DeathState)
    assert(death.size === 4)
    death foreach {
      _.imagePath shouldBe Some("/images/units/oneImageSoldier/im14.png")
    }
  }

  test("all/succ is optional") {
    val vt = SoldierTypeViewInfo("testSoldier2")
    val attacks = vt.attacks
    attacks should have size 1
    val attack = attacks.head
    attack.imageName shouldBe "111"
    attack.projectile shouldBe Some("testProjectile3")

    for(d <- Direction.list; b <- List(true, false)) {
      val images = vt.images(SoldierViewAttackState(b, d, 0))
      images should have size 4
      images(0).imagePath shouldBe Some("/images/units/testSoldier2/im14.png")
      images(1).imagePath shouldBe Some("/images/units/testSoldier2/im15.png")
      images(2).imagePath shouldBe Some("/images/units/testSoldier2/im16.png")
      images(3).imagePath shouldBe Some("/images/units/testSoldier2/im17.png")
    }

    val idle = vt.images(IdleState)
    assert(idle.size === 3)
    assert(idle(0).imagePath.get === "/images/units/testSoldier2/im15.png")
    assert(idle(1).imagePath.get === "/images/units/testSoldier2/im16.png")
    assert(idle(2).imagePath.get === "/images/units/testSoldier2/im17.png")


    val move = vt.images(MoveState)
    assert(move.size === 3)
    assert(move(0).imagePath.get === "/images/units/testSoldier2/im15.png")
    assert(move(1).imagePath.get === "/images/units/testSoldier2/im16.png")
    assert(move(2).imagePath.get === "/images/units/testSoldier2/im17.png")

    val defence = vt.images(DefenceState)
    assert(defence.size === 3)
    assert(defence(0).imagePath.get === "/images/units/testSoldier2/im15.png")
    assert(defence(1).imagePath.get === "/images/units/testSoldier2/im16.png")
    assert(defence(2).imagePath.get === "/images/units/testSoldier2/im17.png")

    val stand = vt.images(StandState)
    assert(stand.size === 3)
    assert(stand(0).imagePath.get === "/images/units/testSoldier2/im15.png")
    assert(stand(1).imagePath.get === "/images/units/testSoldier2/im16.png")
    assert(stand(2).imagePath.get === "/images/units/testSoldier2/im17.png")

    val death = vt.images(DeathState)
    assert(death.size === 4)
    death foreach {
      _.imagePath shouldBe Some("/images/units/testSoldier2/im15.png")
    }
  }
}