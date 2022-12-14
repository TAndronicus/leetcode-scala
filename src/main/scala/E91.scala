object E91 {
  def numDecodings(s: String): Int = {
    if (s(0) == '0') return 0
    val length = s.length
    if (length == 1) return 1
    val dp = new Array[Int](length + 1)
    dp(0) = 1
    dp(1) = 1
    for (i <- 2 until length + 1) {
      if (s(i - 1) != '0') dp(i) += dp(i - 1)
      if (isDecodable(s.substring(i - 2, i).toInt)) dp(i) += dp(i - 2)
      if (dp(i) == 0) return 0
    }
    dp(length)
  }

  private def isDecodable(i: Int): Boolean = {
    i >= 10 && i <= 26
  }

  def main(args: Array[String]): Unit = {
    println(numDecodings("12") == 2)
    println(numDecodings("226") == 3)
    println(numDecodings("06") == 0)
    println(numDecodings("1111") == 5)
    println(numDecodings("11111") == 8)
    println(numDecodings("11110") == 3)
    println(numDecodings("111110") == 5)
    println(numDecodings("1111101") == 5)
    println(numDecodings("11111011") == 10)
    println(numDecodings("11111001") == 0)
    println(numDecodings("111110201") == 5)
    println(numDecodings("111110301") == 0)
  }

}
