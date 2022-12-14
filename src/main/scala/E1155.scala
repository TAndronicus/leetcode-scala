object E1155 {
  def numRollsToTarget(n: Int, k: Int, target: Int): Int = {
    if (target < n || target > k * n) return 0
    if (target == n || target == k * n) return 1
    val dp = new Array[Array[Long]](n)
    val maxRange = target + k
    dp(0) = Array.fill(maxRange)(0)
    for (i <- 0 until k) dp(0)(i) = 1
    for (i <- 1 until n) {
      val row = new Array[Long](maxRange)
      var sum = 0L
      for (j <- i until maxRange) {
        if (j - k - 1 >= 0) sum -= dp(i - 1)(j - k - 1)
        sum = (sum + dp(i - 1)(j - 1)) % 1_000_000_007
        row(j) = if (sum < 0) 1_000_000_007 + sum else sum
      }
      dp(i) = row
    }
    dp(n - 1)(target - 1).toInt
  }

  def main(args: Array[String]): Unit = {
    println(numRollsToTarget(1, 6, 3) == 1)
    println(numRollsToTarget(2, 6, 7) == 6)
    println(numRollsToTarget(3, 6, 4) == 3)
    println(numRollsToTarget(3, 6, 6) == 10)
    println(numRollsToTarget(3, 6, 20) == 0)
    println(numRollsToTarget(3, 6, 2) == 0)
    println(numRollsToTarget(3, 6, 18) == 1)
    println(numRollsToTarget(1, 16, 16) == 1)
    println(numRollsToTarget(1, 16, 17) == 0)
    println(numRollsToTarget(5, 16, 80) == 1)
    println(numRollsToTarget(30, 30, 500) == 222616187)
    println(numRollsToTarget(20, 19, 233) == 378846878)
  }

}
