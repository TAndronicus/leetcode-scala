object BW93E3 {
  private val LAST_RIGHT = 0
  private val LAST_LEFT = 1
  private val DIFF_RIGHT = 2
  private val DIFF_LEFT = 3

  def maxJump(stones: Array[Int]): Int = {
    val dp = Array.fill(stones.length)(Array.fill(2)(Array.fill(4)(0)))
    for (i <- 1 until stones.length) {
      // rightStone
      val (rightDiffRight, rightDiffLeft, leftDiffRight, leftDiffLeft) = (
        stones(i) - dp(i - 1)(0)(LAST_RIGHT),
        stones(i) - dp(i - 1)(1)(LAST_RIGHT),
        stones(i) - dp(i - 1)(0)(LAST_LEFT),
        stones(i) - dp(i - 1)(1)(LAST_LEFT)
      )
      val (maxDiffRight, maxDiffLeft) = (math.max(rightDiffRight, leftDiffRight), math.max(rightDiffLeft, leftDiffLeft))
      if (maxDiffRight < maxDiffLeft) {
        dp(i)(0)(LAST_RIGHT) = stones(i)
        dp(i)(0)(LAST_LEFT) = dp(i - 1)(0)(LAST_LEFT)
        dp(i)(0)(DIFF_RIGHT) = math.max(dp(i - 1)(0)(DIFF_RIGHT), rightDiffRight)
        dp(i)(0)(DIFF_LEFT) = math.max(dp(i - 1)(0)(DIFF_LEFT), leftDiffRight)
        dp(i)(1)(LAST_RIGHT) = dp(i - 1)(0)(LAST_RIGHT)
        dp(i)(1)(LAST_LEFT) = stones(i)
        dp(i)(1)(DIFF_RIGHT) = math.max(dp(i - 1)(0)(DIFF_RIGHT), rightDiffRight)
        dp(i)(1)(DIFF_LEFT) = math.max(dp(i - 1)(0)(DIFF_LEFT), leftDiffRight)
      } else {
        dp(i)(0)(LAST_RIGHT) = stones(i)
        dp(i)(0)(LAST_LEFT) = dp(i - 1)(0)(LAST_LEFT)
        dp(i)(0)(DIFF_RIGHT) = math.max(dp(i - 1)(0)(DIFF_RIGHT), rightDiffLeft)
        dp(i)(0)(DIFF_LEFT) = math.max(dp(i - 1)(0)(DIFF_LEFT), leftDiffLeft)
        dp(i)(1)(LAST_RIGHT) = dp(i - 1)(0)(LAST_RIGHT)
        dp(i)(1)(LAST_LEFT) = stones(i)
        dp(i)(1)(DIFF_RIGHT) = math.max(dp(i - 1)(0)(DIFF_RIGHT), rightDiffLeft)
        dp(i)(1)(DIFF_LEFT) = math.max(dp(i - 1)(0)(DIFF_LEFT), leftDiffLeft)
      }
    }
    math.min(
      math.max(
        dp(stones.length - 1)(0)(DIFF_RIGHT),
        dp(stones.length - 1)(0)(DIFF_LEFT)
      ),
      math.max(
        dp(stones.length - 1)(1)(DIFF_RIGHT),
        dp(stones.length - 1)(1)(DIFF_LEFT)
      )
    )
  }

  def main(args: Array[String]): Unit = {
    println(maxJump(Array(0, 2, 5, 6, 7)))
    println(maxJump(Array(0, 3, 9)))
    println(maxJump(Array(0, 3)))
    println(maxJump(Array(0, 3, 6, 9)))
  }

}
