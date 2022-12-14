
object E268 {
  def missingNumber(nums: Array[Int]): Int = {
    nums.foldLeft(nums.length * (nums.length + 1) / 2) { case (acc, el) => acc - el }
  }


  def main(args: Array[String]): Unit = {
    println(missingNumber(Array(3, 0, 1)) == 2)
    println(missingNumber(Array(0, 1)) == 2)
    println(missingNumber(Array(9, 6, 4, 2, 3, 5, 7, 0, 1)) == 8)
  }

}
