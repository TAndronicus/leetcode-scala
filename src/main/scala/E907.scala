object E907 {
  val MOD = 1000000007

  def sumSubarrayMins(arr: Array[Int]): Int = {
    if (arr.length == 1) return arr(0)
    val n = arr.length
    val dp = Array.fill(n)(Array.fill(n)(0))
    var sum = 0
    for (i <- arr.indices) {
      dp(0)(i) = arr(i)
      sum = (sum + arr(i)) % MOD
    }
    for (i <- 1 until n) {
      for (j <- 0 until (n - i)) {
        dp(i)(j) = Math.min(dp(i - 1)(j), arr(j + i))
        sum = (sum + dp(i)(j)) % MOD
      }
    }
    sum
  }


  def main(args: Array[String]): Unit = {
    println(sumSubarrayMins(Array(2)) == 2)
    println(sumSubarrayMins(Array(3, 1, 2, 4)) == 17)
    println(sumSubarrayMins(Array(11, 81, 94, 43, 3)) == 444)
  }

}
