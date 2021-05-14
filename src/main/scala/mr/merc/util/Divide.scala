package mr.merc.util

object Divide {

  implicit class DivideIntegral[T](i: T)(implicit  num: Integral[T]) {

    def divList(d: T): List[T] = {
      val div = num.quot(i, d)
      val rem = num.rem(i, d)
      val list = List.fill(num.toInt(rem))(num.one) ::: List.fill(num.toInt(d) - num.toInt(rem))(num.zero)
      list.map(a => num.plus(a, div))
    }

    def divList(d: List[Double]): List[T] = {
      require(d.sum != 0, "Sum cann't be zero")
      val norm = normalize(d)
      val multiplied = norm.map(q => (num.toDouble(i) * q).toInt)

      val diff = num.toInt(i) - multiplied.sum

      (multiplied.take(diff).map(_ + 1) ++ multiplied.drop(diff)).map(v => num.fromInt(v))
    }

    private def normalize(seq: List[Double]): List[Double] = {
      val sum = seq.sum
      if (sum == 0) seq
      else {
        val m = 1 / sum
        seq.map(_ * m)
      }
    }
  }
}
