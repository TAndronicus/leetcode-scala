
object E283 {
  def moveZeroes(nums: Array[Int]): Unit = {
    var index = 0
    for (i <- nums.indices) {
      if (nums(i) != 0) {
        if (index != i) nums(index) = nums(i)
        index += 1
      }
    }
    while (index < nums.length) {
      nums(index) = 0
      index += 1
    }
  }


  def main(args: Array[String]): Unit = {
    testArray(Array(0, 1, 0, 3, 12), Array(1, 3, 12, 0, 0))
    testArray(Array(0), Array(0))
  }

  private def testArray(ar: Array[Int], expected: Array[Int]): Unit = {
    moveZeroes(ar)
    println(ar sameElements expected)
  }
}
