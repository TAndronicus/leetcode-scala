object E322 {
  def coinChange(coins: Array[Int], amount: Int): Int = {
    val dp = Array.fill(amount + 1) {
      Int.MaxValue
    }
    dp(0) = 0
    for {
      i <- 1 until amount + 1
      coin <- coins if coin <= i
    } {
      dp(i) = math.min(dp(i - coin), dp(i) - 1) + 1
    }
    if (dp(amount) == Int.MaxValue) -1 else dp(amount)
  }

  def main(args: Array[String]): Unit = {
    println(coinChange(Array(1, 2, 5), 11) == 3)
    println(coinChange(Array(1, 2, 5), 11))
    println(coinChange(Array(2), 3) == -1)
    println(coinChange(Array(2), 0) == 0)
  }

}
