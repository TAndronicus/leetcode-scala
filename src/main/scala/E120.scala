
object E120 {
  def minimumTotal(triangle: List[List[Int]]): Int = {
    if (triangle.size == 1) triangle.head.head
    else if (triangle.size == 2) triangle.head.head + triangle.last.min
    else {
      var lastRow = triangle(1).map(_ + triangle.head.head)
      for (nextRow <- triangle.drop(2)) {
        lastRow = nextRow.zip(List(lastRow.head) :: lastRow.sliding(2).toList ::: List(List(lastRow.last)))
          .map { case (nextValue, lastPath) => nextValue + lastPath.min }
      }
      lastRow.min
    }
  }

  def main(args: Array[String]): Unit = {
    println(minimumTotal(List(List(-10))) == -10)
    println(minimumTotal(List(List(-10), List(1, 2))) == -9)
    println(minimumTotal(List(List(2), List(3, 4), List(6, 5, 7), List(4, 1, 8, 3))) == 11)
    println(minimumTotal(List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4), List(9, 9, 3, 2, 1))) == 8)
  }

}
