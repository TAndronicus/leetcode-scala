object BW88E4 {
  def numberOfPairs(nums1: Array[Int], nums2: Array[Int], diff: Int): Long = {
    val diffs = nums1.zip(nums2).map { case (left, right) => left - right }.sorted
    var (i1, i2) = (0, 1)
    var res = 0
    while (i2 < diffs.length) {
      if (diffs(i1) + diff <= diffs(i2)) {
        res += i1 + 1; i1 += 1
      }
      else i2 += 1
    }
    res
  }

  def main(args: Array[String]): Unit = {
    println(numberOfPairs(Array(3, 2, 5), Array(2, 2, 1), 1))
  }

}
