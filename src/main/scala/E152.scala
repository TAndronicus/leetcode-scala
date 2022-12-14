object E152 {
  def maxProduct1(nums: Array[Int]): Int = {
    var (start, end, previousZero, max, hasZero) = (-1, -1, true, Int.MinValue, false)
    for (i <- nums.indices) {
      if (nums(i) != 0) {
        if (previousZero) {
          previousZero = false
          start = i
        }
        end = i
      } else {
        if (!hasZero) hasZero = true
        if (!previousZero) {
          previousZero = true
          max = math.max(maxProdNonZero(nums, start, end), max)
        }
      }
    }
    if (end == nums.length - 1) max = math.max(max, maxProdNonZero(nums, start, end))
    if (max < 0 && hasZero) 0
    else max
  }

  private def maxProdNonZero(nums: Array[Int], start: Int, end: Int): Int = {
    val len = end - start + 1
    if (len == 1) return nums(start)
    var max = Int.MinValue
    val dp = Array.fill(len) {
      Array.fill(len)(0)
    }
    for (i <- 0 until len) {
      dp(i)(0) = nums(start + i)
      max = math.max(max, dp(i)(0))
    }
    for {
      i <- 1 until len
      j <- 1 to i
    } {
      dp(i)(j) = dp(i - 1)(j - 1) * nums(start + i)
      max = math.max(max, dp(i)(j))
    }
    max
  }

  def maxProduct2(nums: Array[Int]): Int = {
    if (nums.length == 1) return nums(0)
    var (localMax, localMin, globalMax) = (nums(0), nums(0), nums(0))
    var (preMax, preMin) = (localMax, localMin)
    for (i <- 1 until nums.length) {
      preMax = math.max(math.max(localMax * nums(i), localMin * nums(i)), nums(i))
      preMin = math.min(math.min(localMax * nums(i), localMin * nums(i)), nums(i))
      globalMax = math.max(preMax, globalMax)
      localMax = preMax
      localMin = preMin
    }
    globalMax
  }

  def maxProduct3(nums: Array[Int]): Int = {
    if (nums.length == 1) nums(0)
    else {
      val (localMax, _, globalMax) = nums.tail
        .foldLeft((nums(0), nums(0), nums(0))) { case ((preMax, preMin, glMax), el) => (
          math.max(math.max(preMax * el, preMin * el), el),
          math.min(math.min(preMax * el, preMin * el), el),
          math.max(preMax, glMax)
        )
        }
      math.max(localMax, globalMax)
    }
  }

  def main(args: Array[String]): Unit = {
    println(maxProduct1(Array(3, -1, 4)) == 4)
    println(maxProduct1(Array(2, 3, -2, 4)) == 6)
    println(maxProduct1(Array(-2, 0, -1)) == 0)
    println(maxProduct1(Array(-2, 0, -1, -1)) == 1)
    println(maxProduct1(Array(-2, -1, 0, -1, -1)) == 2)
    println(maxProduct1(Array(-2, 1, 0, 1, -1)) == 1)
    println(maxProduct1(Array(-2, 1, -1, 1, -1)) == 2)
    println(maxProduct1(Array(-2, -1, -1, -1, -1)) == 2)
    println(maxProduct1(Array(0)) == 0)
    println(maxProduct1(Array(-1)) == -1)
    println(maxProduct1(Array(1)) == 1)
    println(maxProduct1(Array(2, -5, -2, -4, 3)) == 24)
    println(maxProduct2(Array(2, -5, -2, -4, 3)) == 24)
    println(maxProduct2(Array(3, -1, 4)) == 4)
    println(maxProduct2(Array(2, 3, -2, 4)) == 6)
    println(maxProduct2(Array(-2, 0, -1)) == 0)
    println(maxProduct2(Array(-2, 0, -1, -1)) == 1)
    println(maxProduct2(Array(-2, -1, 0, -1, -1)) == 2)
    println(maxProduct2(Array(-2, 1, 0, 1, -1)) == 1)
    println(maxProduct2(Array(-2, 1, -1, 1, -1)) == 2)
    println(maxProduct2(Array(-2, -1, -1, -1, -1)) == 2)
    println(maxProduct2(Array(0)) == 0)
    println(maxProduct2(Array(-1)) == -1)
    println(maxProduct2(Array(1)) == 1)
    println(maxProduct3(Array(2, -5, -2, -4, 3)) == 24)
    println(maxProduct3(Array(3, -1, 4)) == 4)
    println(maxProduct3(Array(2, 3, -2, 4)) == 6)
    println(maxProduct3(Array(2, 3, -2, 4)) == 6)
    println(maxProduct3(Array(-2, 0, -1)) == 0)
    println(maxProduct3(Array(-2, 0, -1, -1)) == 1)
    println(maxProduct3(Array(-2, -1, 0, -1, -1)) == 2)
    println(maxProduct3(Array(-2, 1, 0, 1, -1)) == 1)
    println(maxProduct3(Array(-2, 1, -1, 1, -1)) == 2)
    println(maxProduct3(Array(-2, -1, -1, -1, -1)) == 2)
    println(maxProduct3(Array(0)) == 0)
    println(maxProduct3(Array(-1)) == -1)
    println(maxProduct3(Array(1)) == 1)
  }

}
