package mr.merc.local


import org.scalatest.FunSuite

class LocalizationTest extends FunSuite {

  test("message without parameters") {
    val actualMessage: String = Localization("default.message")

    assert(actualMessage === "Default Message")
  }

  test("message with one parameter") {
    val actualMessage: String = Localization("hello.world", "world")

    assert(actualMessage === "Hello world!")
  }

  test("message with two parameters of different types") {
    val actualMessage: String = Localization("new.mail.received", "Harry", 2)

    assert(actualMessage === "Hi Harry, you have 2 new emails")
  }

  test("not existing message") {
    val actualMessage: String = Localization("not.existing.message")

    assert(actualMessage === "not.existing.message")
  }
}
