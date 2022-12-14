import scala.collection.mutable.ArrayBuffer

object E73 {
  def setZeroes(matrix: Array[Array[Int]]): Unit = {
    val (rows, cols) = (ArrayBuffer[Int](), ArrayBuffer[Int]())
    for (i <- matrix.indices) {
      for (j <- matrix(0).indices) {
        if (matrix(i)(j) == 0) {
          rows.append(i)
          cols.append(j)
        }
      }
    }
    for (i <- rows; j <- matrix(0).indices) {
      matrix(i)(j) = 0
    }
    for (i <- matrix.indices; j <- cols) {
      matrix(i)(j) = 0
    }
  }


  def main(args: Array[String]): Unit = {
    assertZeroed(Array(Array(1, 1, 1), Array(1, 0, 1), Array(1, 1, 1)), Array(Array(1, 0, 1), Array(0, 0, 0), Array(1, 0, 1)))
    assertZeroed(Array(Array(0, 1, 2, 0), Array(3, 4, 5, 2), Array(1, 3, 1, 5)), Array(Array(0, 0, 0, 0), Array(0, 4, 5, 0), Array(0, 3, 1, 0)))
    assertZeroed(Array(Array(0, 0, 0, 5), Array(4, 3, 1, 4), Array(0, 1, 1, 4), Array(1, 2, 1, 3), Array(0, 0, 1, 1)), Array(Array(0, 0, 0, 0), Array(0, 0, 0, 4), Array(0, 0, 0, 0), Array(0, 0, 0, 3), Array(0, 0, 0, 0)))
  }

  def assertZeroed(matrix: Array[Array[Int]], result: Array[Array[Int]]): Unit = {
    printMatrix(matrix)
    setZeroes(matrix)
    println("---")
    printMatrix(matrix)
    println(matrix.zip(result).forall { case (orig, res) => orig sameElements res })
  }

  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    for (row <- matrix) {
      println(row.mkString(" "))
    }
  }

}
