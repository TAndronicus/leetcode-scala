object BW91E1 {

  import scala.collection.mutable

  def distinctAverages(nums: Array[Int]): Int = {
    nums.sortInPlace()
    val s = mutable.Set[Double]()
    for (i <- 0 until nums.length / 2) s.add((nums(i) + nums(nums.length - 1 - i)) / 2.0)
    s.size
  }

  def main(args: Array[String]): Unit = {
    println(distinctAverages(Array(4, 1, 4, 0, 3, 5)) == 2)
    println(distinctAverages(Array(1, 100)) == 1)
  }

}
