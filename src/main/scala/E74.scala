object E74 {
  def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
    import java.util.Arrays.binarySearch
    val row = binarySearch(matrix.map(_.head), target)
    matrix(0)(0) <= target && (row >= 0 || binarySearch(matrix(math.abs(row) - 2), target) >= 0)
  }

  def main(args: Array[String]): Unit = {
    println(searchMatrix(Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)), 3))
    println(!searchMatrix(Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)), 13))
    println(searchMatrix(Array(Array(1, 1), Array(2, 2)), 1))
    println(!searchMatrix(Array(Array(1, 1), Array(2, 2)), 3))
    println(searchMatrix(Array(Array(1, 1), Array(2, 2)), 2))
  }

}
