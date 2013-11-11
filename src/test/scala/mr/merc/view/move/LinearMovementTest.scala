package mr.merc.view.move

import org.scalatest.FunSuite

class LinearMovementTest extends FunSuite {
     
    def createMovement:LinearMovement = {
      val x1 = 10
	  val y1 = 20
	  val x2 = 40 // 30 is width
	  val y2 = 60 // 40 is height, then 50 is diagonal
	  val speed = 10 // 10 pixels per second, 
	  			     // 5 seconds to cover the distance
	  
	  new LinearMovement(x1, y1, x2, y2, speed)
    }
 
	test("ideal movement") {
	  val movement = createMovement
	  movement.start()
	  movement.update(2500)
	  assert(movement.x === 10 + 15)
	  assert(movement.y === 20 + 20)
	  assert(!movement.isOver)
	  movement.update(2500)
	  assert(movement.x === 40)
	  assert(movement.y === 60)
	  assert(movement.isOver)
	}
	
	test("time overflow should not lead to coordinates overflow") {
	  val movement = createMovement
	  movement.start()
	  movement.update(5500)
	  assert(movement.x === 40)
	  assert(movement.y === 60)
	  assert(movement.isOver)
	}
	
	test("percentage") {	  
	  val movement = new LinearMovement(10, 20, 40, 60, 10, 0.8)
	  assert(movement.destination === (34, 52))
	  movement.start()
	  movement.update(6000)
	  assert(movement.x === 34)
	  assert(movement.y === 52)
	  
	}
}