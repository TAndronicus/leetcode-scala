import scala.collection.mutable

object E414 {
  def thirdMax(nums: Array[Int]): Int = {
    if (nums.length == 1) nums(0)
    if (nums.length == 2) if (nums(0) > nums(1)) nums(0) else nums(1)
    val acc = mutable.TreeSet[Int]()
    for (el <- nums) {
      if (acc.size < 3 || el > acc.min) {
        acc.add(el)
        if (acc.size == 4) acc.remove(acc.min)
      }
    }
    if (acc.size == 3) acc.min else acc.max
  }


  def main(args: Array[String]): Unit = {
    println(thirdMax(Array(3, 2, 1)) == 1)
    println(thirdMax(Array(1, 2)) == 2)
    println(thirdMax(Array(2, 2, 3, 1)) == 1)
    println(thirdMax(Array(2, 2, 3, 2)) == 3)
    println(thirdMax(Array(1, 2, 2, 5, 3, 5)) == 2)
  }

}
