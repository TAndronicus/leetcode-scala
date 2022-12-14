object E1235 {

  import scala.collection.mutable

  def jobScheduling(startTime: Array[Int], endTime: Array[Int], profit: Array[Int]): Int = {
    val m = mutable.HashMap[Int, mutable.Set[(Int, Int)]]()
    val dp = mutable.TreeMap[Int, Int]()
    for (i <- startTime.indices) {
      if (m.contains(endTime(i))) m(endTime(i)).add((startTime(i), profit(i)))
      else m(endTime(i)) = mutable.Set((startTime(i), profit(i)))
    }
    var max = 0
    for (end <- endTime.sorted) {
      for ((start, prof) <- m(end)) {
        dp(end) = Math.max(dp.maxBefore(end + 1).map(_._2).getOrElse(0), dp.maxBefore(start + 1).map(_._2).getOrElse(0) + prof)
        max = Math.max(max, dp(end))
      }
    }
    max
  }

  def main(args: Array[String]): Unit = {
    println(jobScheduling(Array(1, 2, 3, 3), Array(3, 4, 5, 6), Array(50, 10, 40, 70)))
    println(jobScheduling(Array(1, 2, 3, 4, 6), Array(3, 5, 10, 6, 9), Array(20, 20, 100, 70, 60)))
    println(jobScheduling(Array(1, 1, 1), Array(2, 3, 4), Array(5, 6, 4)))
  }

}
