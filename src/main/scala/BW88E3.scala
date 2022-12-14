object BW88E3 {
  def xorAllNums(nums1: Array[Int], nums2: Array[Int]): Int = {
    if (nums1.length % 2 == 0 && nums2.length % 2 == 0) 0
    else if (nums1.length % 2 == 0) nums1.fold(0) { case (acc, el) => acc ^ el }
    else if (nums2.length % 2 == 0) nums2.fold(0) { case (acc, el) => acc ^ el }
    else nums1.fold(0) { case (acc, el) => acc ^ el } ^ nums2.fold(0) { case (acc, el) => acc ^ el }
  }

  def main(args: Array[String]): Unit = {
    println(xorAllNums(Array(2, 1, 3), Array(10, 2, 5, 0)) == 13)
    println(xorAllNums(Array(2, 1, 3), Array(10, 2, 5, 0)))
    println(xorAllNums(Array(1, 2), Array(3, 4)) == 0)
  }

}
