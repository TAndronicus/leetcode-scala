object BW93E4 {

  import scala.collection.mutable

  def minimumTotalCost(nums1: Array[Int], nums2: Array[Int]): Long = {
    val toChange = mutable.HashMap[Int, Int]()
    val overall1 = mutable.HashMap[Int, Int]()
    val overall2 = mutable.HashMap[Int, Int]()
    var sumIndices = 0L
    var numIndices = 0
    for (i <- nums1.indices) {
      val (e1, e2) = (nums1(i), nums2(i))
      overall1.updateWith(e1)(_.map(_ + 1).orElse(Some(1)))
      overall2.updateWith(e2)(_.map(_ + 1).orElse(Some(1)))
      if (e1 == e2) {
        toChange.updateWith(e1)(_.map(_ + 1).orElse(Some(1)))
        sumIndices += i
        numIndices += 1
      }
    }
    val maj1 = overall1.find { case (_, l) => l > nums1.length / 2 }.map(_._1)
    val maj2 = overall2.find { case (_, l) => l > nums1.length / 2 }.map(_._1)
    if (maj1.nonEmpty && maj2.nonEmpty && maj1.get == maj2.get) -1L
    else {
      val maj = toChange.find { case (_, l) => l > numIndices / 2 }
      if (maj.isEmpty) sumIndices
      else {
        var left = 2 * maj.get._2 - numIndices
        var overhead = 0L
        var index = 0
        while (left > 0) {
          if (nums1(index) != maj.get._1) {
            overhead += index
            left -= 1
          }
          index += 1
        }
        sumIndices + overhead
      }
    }
  }

  def main(args: Array[String]): Unit = {
  }

}
