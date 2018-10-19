package mr.merc.util

object Divide {

  implicit class DivideIntegral[T](i: T)(implicit  num: Integral[T]) {

    def divList(d: T): List[T] = {
      val div = num.quot(i, d)
      val rem = num.rem(i, d)
      val list = List.fill(num.toInt(rem))(num.one) ::: List.fill(num.toInt(d) - num.toInt(rem))(num.zero)
      list.map(a => num.plus(a, div))
    }
  }
}
