object E75 {
  def sortColors(nums: Array[Int]): Unit = {
    var (index, one, two) = (0, nums.length - 1, nums.length - 1)
    while (index <= one) {
      if (nums(index) == 0) {
        index += 1
      } else if (nums(index) == 1) {
        nums(index) = nums(one)
        nums(one) = 1
        one -= 1
      } else {
        nums(index) = nums(one)
        nums(one) = 1
        nums(two) = 2
        one -= 1
        two -= 1
      }
    }
  }


  def main(args: Array[String]): Unit = {
    println(assertSortColors(Array(0), Array(0)))
    println(assertSortColors(Array(1), Array(1)))
    println(assertSortColors(Array(2), Array(2)))
    println(assertSortColors(Array(0, 1, 2), Array(0, 1, 2)))
    println(assertSortColors(Array(2, 1, 2), Array(1, 2, 2)))
    println(assertSortColors(Array(2, 1, 2, 0), Array(0, 1, 2, 2)))
    println(assertSortColors(Array(2, 0, 2, 0, 1, 0, 1), Array(0, 0, 0, 1, 1, 2, 2)))
    println(assertSortColors(Array(2, 0, 2, 0, 1, 0, 1, 0), Array(0, 0, 0, 0, 1, 1, 2, 2)))
    println(assertSortColors(Array(0, 1, 2, 1, 0), Array(0, 0, 1, 1, 2)))
    println(assertSortColors(Array(2, 1, 0, 1, 2), Array(0, 1, 1, 2, 2)))
    println(assertSortColors(Array(0, 2, 0, 1, 0, 2, 0), Array(0, 0, 0, 0, 1, 2, 2)))
  }

  def assertSortColors(original: Array[Int], expected: Array[Int]): Boolean = {
    sortColors(original)
    original sameElements expected
  }

}
