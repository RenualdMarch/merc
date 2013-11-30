package mr.merc.conf

import org.scalatest.FunSuite
import java.io.File

class ConfTest extends FunSuite {
  test("Main test") {
    // file is absent
    val conf = Conf.readProperties
    
    // default is returned
    assert(conf.properties === Conf.defaultConf)
    
    Conf.writeChanges(Map("prop1" -> "value1"))
    Thread.sleep(500)
    
    val conf2 = Conf.readProperties
    assert(conf2.properties === Conf.defaultConf + ("prop1" -> "value1"))
    
    val file = new File(Conf.confPath)
    assert(file.delete())
  }

}