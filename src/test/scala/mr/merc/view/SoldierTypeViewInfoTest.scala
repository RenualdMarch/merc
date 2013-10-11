package mr.merc.view

import org.scalatest.FunSuite
import mr.merc.unit.view.SoldierTypeViewInfo
import mr.merc.unit.view.IdleState
import mr.merc.unit.view.StandState
import mr.merc.unit.view.DeathState
import mr.merc.unit.view.MoveState
import mr.merc.unit.view.DefenceState
import org.scalatest.matchers.ShouldMatchers
import mr.merc.unit.view.FirstAttackNSuccState
import mr.merc.unit.view.FirstAttackNFailState
import mr.merc.unit.view.FirstAttackNEFailState
import mr.merc.unit.view.FirstAttackNESuccState
import mr.merc.unit.view.FirstAttackSESuccState
import mr.merc.unit.view.FirstAttackSEFailState
import mr.merc.unit.view.FirstAttackSSuccState
import mr.merc.unit.view.FirstAttackSFailState
import mr.merc.image.LazyMirroredVerticallyImage
import mr.merc.unit.view.SoldierViewAttackState
import mr.merc.unit.view.FirstAttackNWSuccState
import mr.merc.unit.view.FirstAttackNWFailState
import mr.merc.unit.view.FirstAttackSWSuccState
import mr.merc.unit.view.FirstAttackSWFailState
import mr.merc.map.hex._


class SoldierTypeViewInfoTest extends FunSuite with ShouldMatchers {
  
    test("loading without optional parameters") {
	  val vt = SoldierTypeViewInfo("testType1")
	  val attack1ns = vt.images(FirstAttackNSuccState)
	  assert(attack1ns.size === 2)
	  assert(attack1ns(0).imagePath.get === "/images/units/testType1/im1n.png")
	  assert(attack1ns(0).alpha === 0.5)
	  assert(attack1ns(0).xOffset === 5)
	  assert(attack1ns(0).yOffset === 3)
	  assert(attack1ns(1).imagePath.get === "/images/units/testType1/im2n.png")
	  assert(attack1ns(1).alpha === 1)
	  assert(attack1ns(1).xOffset === 0)
	  assert(attack1ns(1).yOffset === 0)
	  
	  val attack1nf = vt.images(FirstAttackNFailState)
	  assert(attack1nf.size === 1)
	  assert(attack1nf(0).imagePath.get === "/images/units/testType1/im3n.png")
	  
	  val attack1nes = vt.images(FirstAttackNESuccState)
	  assert(attack1nes.size === 2)
	  assert(attack1nes(0).imagePath.get === "/images/units/testType1/im1m.png")
	  assert(attack1nes(1).imagePath.get === "/images/units/testType1/im2m.png")

	  val attack1nef= vt.images(FirstAttackNEFailState)
	  assert(attack1nef.size === 1)
	  assert(attack1nef(0).imagePath.get === "/images/units/testType1/im3m.png")

	  val attack1ses = vt.images(FirstAttackSESuccState)
	  assert(attack1ses.size === 2)
	  assert(attack1ses(0).imagePath.get === "/images/units/testType1/im1r.png")
	  assert(attack1ses(1).imagePath.get === "/images/units/testType1/im2r.png")

	  val attack1sef= vt.images(FirstAttackSEFailState)
	  assert(attack1sef.size === 1)
	  assert(attack1sef(0).imagePath.get === "/images/units/testType1/im3r.png")

	  
	  val attack1ss = vt.images(FirstAttackSSuccState)
	  assert(attack1ss.size === 2)
	  assert(attack1ss(0).imagePath.get === "/images/units/testType1/im1s.png")
	  assert(attack1ss(1).imagePath.get === "/images/units/testType1/im2s.png")

	  val attack1sf= vt.images(FirstAttackSFailState)
	  assert(attack1sf.size === 2)
	  assert(attack1sf(0).imagePath.get === "/images/units/testType1/im1s.png")
	  assert(attack1sf(1).imagePath.get === "/images/units/testType1/im2s.png")
	  
	  val attack1sws = vt.images(FirstAttackSWSuccState)
	  assert(attack1sws.size === 2)
	  assert(attack1sws(0).isInstanceOf[LazyMirroredVerticallyImage])
	  assert(attack1sws(1).isInstanceOf[LazyMirroredVerticallyImage])

	  val attack1swf= vt.images(FirstAttackSWFailState)
	  assert(attack1swf.size === 1)
	  assert(attack1swf(0).isInstanceOf[LazyMirroredVerticallyImage])

	  val attack1nws = vt.images(FirstAttackNWSuccState)
	  assert(attack1nws.size === 2)
	  assert(attack1nws(0).isInstanceOf[LazyMirroredVerticallyImage])
	  assert(attack1nws(1).isInstanceOf[LazyMirroredVerticallyImage])

	  val attack1nwf= vt.images(FirstAttackNWFailState)
	  assert(attack1nwf.size === 1)
	  assert(attack1nwf(0).isInstanceOf[LazyMirroredVerticallyImage])
	  
	  
	  for (s <- SoldierViewAttackState.all if s.number == 1) {
	    val attack2 = vt.images(s)
	    val configurable = Set(N, NE, SE, S)
	    
	    if (s.success) {
	      assert(attack2.size === 2)
	      if (configurable.contains(s.direction)) {
	        assert(attack2(0).imagePath.get === "/images/units/testType1/im4a.png")
	        assert(attack2(1).imagePath.get === "/images/units/testType1/im5a.png")
	      } else {
	        attack2.foreach(p => assert(p.isInstanceOf[LazyMirroredVerticallyImage]))
	      }
	    } else {
	      assert(attack2.size === 1)
	      if (configurable.contains(s.direction)) {
	        assert(attack2(0).imagePath.get === "/images/units/testType1/im6a.png")
	      } else {
	        attack2.foreach(p => assert(p.isInstanceOf[LazyMirroredVerticallyImage]))
	      }
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
	  death(0).alpha should be (0.7f plusOrMinus 0.01f)
	  assert(death(1).imagePath.get === "/images/units/testType1/im11.png")
	  death(1).alpha should be (0.3f plusOrMinus 0.01f)
	  
	  val move = vt.images(MoveState)
	  assert(move.size === 1)
	  assert(move(0).imagePath.get === "/images/units/testType1/im12.png")
	  
	  val defence = vt.images(DefenceState)
	  assert(defence.size === 1)
	  assert(defence(0).imagePath.get === "/images/units/testType1/im13.png")
	}
    
    
	
	test("loading using only required parameters") {
	  val vt = SoldierTypeViewInfo("testType2")
	  for (s <- SoldierViewAttackState.all) {
	    val attack = vt.images(s)
	    val configurable = Set(N, NE, SE, S)
	    
	    assert(attack.size === 1)
	    if (configurable.contains(s.direction)) {
	      assert(attack(0).imagePath.get === "/images/units/testType2/im14.png")
	      assert(attack(0).alpha === 1)
	      assert(attack(0).xOffset === 0)
	      assert(attack(0).yOffset === 0)
	    } else {
	      attack.foreach(p => assert(p.isInstanceOf[LazyMirroredVerticallyImage]))
	    }	   
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
	  assert(death.size === 5)
	  assert(death(0).imagePath.get === "/images/units/testType2/im15.png")
	  death(0).alpha should be (1f plusOrMinus 0.01f)
	  assert(death(1).imagePath.get === "/images/units/testType2/im15.png")
	  death(1).alpha should be (0.8f plusOrMinus 0.01f)
	  assert(death(2).imagePath.get === "/images/units/testType2/im15.png")
	  death(2).alpha should be (0.6f plusOrMinus 0.01f)
	  assert(death(3).imagePath.get === "/images/units/testType2/im15.png")
	  death(3).alpha should be (0.4f plusOrMinus 0.01f)
	  assert(death(4).imagePath.get === "/images/units/testType2/im15.png")
	  death(4).alpha should be (0.2f plusOrMinus 0.01f)
	}
}