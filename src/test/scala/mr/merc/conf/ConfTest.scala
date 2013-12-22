package mr.merc.conf

import org.scalatest.FunSuite
import java.io.File
import org.scalatest.BeforeAndAfter

class ConfTest extends FunSuite with BeforeAndAfter {
  before {
    val file = new File(Conf.confPath)
    if (file.exists()) {
      assert(file.delete())
    }
  }

  test("Main test") {
    // file is absent
    val conf = Conf.readProperties

    // default is returned
    assert(conf.properties === Conf.defaultConf)

    Conf.writeChanges(Map("prop1" -> "value1"), true)
    assert(Conf.string("prop1") === "value1")
    Thread.sleep(500)

    val conf2 = Conf.readProperties
    assert(conf2.properties === Conf.defaultConf + ("prop1" -> "value1"))
  }

  after {
    val file = new File(Conf.confPath)
    assert(file.delete())
  }

}