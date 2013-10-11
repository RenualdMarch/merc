package mr.merc.unit.view

import org.scalatest.FunSuite
import mr.merc.map.hex.N
import mr.merc.map.hex.NE

class ProjectileTest extends FunSuite {
	test("parse projectile config for full case") {
	  val projectile = Projectile("testProjectile")
	  val start = projectile.start
	  assert(start.size === 6)
	  start.values foreach(list =>{
	    assert(list.size === 2)
	    assert(list(0).imagePath.get === "/images/projectiles/testProjectile/im1.png")
	    assert(list(1).imagePath.get === "/images/projectiles/testProjectile/im2.png")
	  })
	  	  
	  val move = projectile.move
	  assert(move.size === 6)
	  move foreach (kv => {
	    assert(kv._2.size === 2)
	    if (kv._1 != N && kv._1 != NE) {
	      assert(kv._2.forall(_.imagePath.isEmpty))
	    }
	  })
	  val moveN = move(N)
	  assert(moveN.size === 2)
	  assert(moveN(0).imagePath.get === "/images/projectiles/testProjectile/im3.png")
	  assert(moveN(1).imagePath.get === "/images/projectiles/testProjectile/im4.png")
	  
	  val moveNE = move(NE)
      assert(moveNE.size === 2)
	  assert(moveNE(0).imagePath.get === "/images/projectiles/testProjectile/im5.png")
	  assert(moveNE(1).imagePath.get === "/images/projectiles/testProjectile/im6.png")
	  
	  val end = projectile.end
	  assert(end.size === 6)
	  end.values foreach(list =>{
	    assert(list.size === 3)
	    assert(list(0).imagePath.get === "/images/projectiles/testProjectile/im7.png")
	    assert(list(1).imagePath.get === "/images/projectiles/testProjectile/im8.png")
	    assert(list(2).imagePath.get === "/images/projectiles/testProjectile/im9.png")
	  })
	}
	
	test("projectile config for short case") {
	  val projectile = Projectile("testProjectile2")
	  val start = projectile.start
	  
	  start.values foreach(list => {
	    assert(list.size === 1)
	    assert(list(0).imagePath.get === "/images/projectiles/testProjectile2/im10.png")
	  })
	  
	  val end = projectile.end
	  end.values foreach(list => {
	    assert(list.size === 1)
	    assert(list(0).imagePath.get === "/images/projectiles/testProjectile2/im11.png")
	  })
	  
	  val move = projectile.move
	  move.values foreach(list => {
	    assert(list.size === 2)
	    assert(list(0).imagePath.get === "/images/projectiles/testProjectile2/im10.png")
	    assert(list(1).imagePath.get === "/images/projectiles/testProjectile2/im11.png")
	  })
	}
}