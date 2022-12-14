object E80 {
  def removeDuplicates(nums: Array[Int]): Int = {
    var (lastItem, timesMatched, index) = (-10001, 2, 0)
    for (el <- nums) {
      if (el == lastItem) timesMatched += 1
      else {
        timesMatched = 0
        lastItem = el
      }
      if (timesMatched < 2) {
        nums(index) = el
        index += 1
      }
    }
    index
  }


  def main(args: Array[String]): Unit = {
    println(assertRemoveDuplicates(Array(1, 1, 1, 2, 2, 3), Array(1, 1, 2, 2, 3, 3), 5))
    println(assertRemoveDuplicates(Array(1), Array(1), 1))
    println(assertRemoveDuplicates(Array(1, 1, 2, 2, 3, 3, 4), Array(1, 1, 2, 2, 3, 3, 4), 7))
    println(assertRemoveDuplicates(Array(0, 0, 1, 1, 1, 1, 2, 3, 3), Array(0, 0, 1, 1, 2, 3, 3), 7))
  }

  def assertRemoveDuplicates(original: Array[Int], expected: Array[Int], expectedLength: Int): Boolean = {
    val actualLength = removeDuplicates(original)
    actualLength == expectedLength && original.zip(expected).take(expectedLength).forall { case (left, right) => left == right }
  }

}
