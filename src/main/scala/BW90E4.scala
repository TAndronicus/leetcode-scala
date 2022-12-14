object BW90E4 {

  import scala.collection.mutable

  def secondGreaterElement(nums: Array[Int]): Array[Int] = {
    val examined = new mutable.TreeSet[Int]()
    val res = Array.fill(nums.length)(0)
    for (i <- nums.length - 1 to 0 by -1) {
      res(i) = examined.minAfter(nums(i))
        .flatMap(firstGreater => examined.minAfter(firstGreater))
        .getOrElse(-1)
      examined.add(nums(i))
    }
    res
  }


  def main(args: Array[String]): Unit = {
    println(secondGreaterElement(Array(2, 4, 0, 9, 6)).mkString(","))
  }

}
