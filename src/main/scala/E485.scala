object E485 {
  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    var max = 0
    var previousMax = 0
    for (el <- nums) {
      if (el == 1) previousMax += 1
      else {
        max = math.max(max, previousMax)
        previousMax = 0
      }
    }
    math.max(max, previousMax)
  }

  def main(args: Array[String]): Unit = {
    println(findMaxConsecutiveOnes(Array(1, 0, 1, 1, 0, 1)) == 2)
  }

}
