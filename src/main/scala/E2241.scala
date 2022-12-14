object E2241 {
  class ATM() {
    private val stored = Array[Long](0, 0, 0, 0, 0)
    private val denominations = Array(20, 50, 100, 200, 500)

    def deposit(banknotesCount: Array[Int]) {
      for (i <- 0 until 5) stored(i) += banknotesCount(i)
    }

    def withdraw(amount: Int): Array[Int] = {
      var left = amount.toLong
      var i = 4
      val res = Array(0, 0, 0, 0, 0)
      while (left != 0 && i >= 0) {
        val subs = math.min(left / denominations(i), stored(i))
        if (subs != 0) {
          left -= subs * denominations(i); stored(i) -= subs; res(i) += subs.toInt
        }
        i -= 1
      }
      if (left == 0) res
      else {
        deposit(res)
        Array(-1)
      }
    }

  }

  def main(args: Array[String]): Unit = {
    val atm1 = new ATM()
    atm1.deposit(Array(0, 0, 1, 2, 1))
    println(atm1.withdraw(600) sameElements Array(0, 0, 1, 0, 1))
    atm1.deposit(Array(0, 1, 0, 1, 1))
    println(atm1.withdraw(600) sameElements Array(-1))
    println(atm1.withdraw(550) sameElements Array(0, 1, 0, 0, 1))
    val atm2 = new ATM()
    atm2.deposit(Array(0, 10, 0, 3, 0))
    println(atm2.withdraw(500) sameElements Array(0, 2, 0, 2, 0))
  }

}
