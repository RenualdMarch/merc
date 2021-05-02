package mr.merc.unit

import org.scalatest.FunSuite
import mr.merc.unit.AttackType._

class AttackTypeTest extends FunSuite{
	test("get by name") {
	  assert(AttackType("Blade") === Blade)
	  assert(AttackType("Impact") === Impact)
	}
}