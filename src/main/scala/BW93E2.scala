object BW93E2 {

  import scala.collection.mutable

  def maxStarSum(vals: Array[Int], edges: Array[Array[Int]], k: Int): Int = {
    if (k == 0) return vals.max
    val m = mutable.HashMap[Int, mutable.ListBuffer[Int]]()
    for (edge <- edges) {
      val (k1, k2) = (edge(0), edge(1))
      val (v1, v2) = (vals(k1), vals(k2))
      if (m.contains(k1)) m(k1).addOne(v2)
      else m(k1) = mutable.ListBuffer(v2)
      if (m.contains(k2)) m(k2).addOne(v1)
      else m(k2) = mutable.ListBuffer(v1)
    }
    var maxSum = vals(0)
    for (n <- vals.indices) {
      maxSum = math.max(
        maxSum,
        vals(n) + m.getOrElse(n, Set[Int]()).toList
          .filter(v => v > 0)
          .sorted
          .reverse
          .take(k)
          .sum
      )
    }
    maxSum
  }

  def main(args: Array[String]): Unit = {
    println(maxStarSum(Array(1, 2, 3, 4, 10, -10, -20), Array(Array(0, 1), Array(1, 2), Array(1, 3), Array(3, 4), Array(3, 5), Array(3, 6)), 2) == 16)
    println(maxStarSum(Array(-5), Array(), 0) == -5)
    println(maxStarSum(Array(-1, 0), Array(), 1) == 0)
    println(maxStarSum(Array(-14, -12, -24, 20, -4), Array(Array(1, 3), Array(2, 4), Array(0, 4), Array(3, 0)), 3) == 20)
  }

}
