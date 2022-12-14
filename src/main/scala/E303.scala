

object E303 {

  import scala.collection.mutable

  class NumArray(_nums: Array[Int]) {
    private val prefixSum: mutable.Map[Int, Int] = mutable.Map((-1, 0))
    for (i <- _nums.indices) prefixSum(i) = prefixSum(i - 1) + _nums(i)

    def sumRange(left: Int, right: Int): Int = {
      prefixSum(right) - prefixSum(left - 1)
    }

  }


  def main(args: Array[String]): Unit = {
    val s = new NumArray(Array(-2, 0, 3, -5, 2, -1))
    println(s.sumRange(0, 2) == 1)
    println(s.sumRange(2, 5) == -1)
    println(s.sumRange(0, 5) == -3)
  }

}
