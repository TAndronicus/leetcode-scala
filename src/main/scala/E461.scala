object E461 {
  def hammingDistance1(x: Int, y: Int): Int = {
    val (left, right) = (Integer.toString(x, 2), Integer.toString(y, 2))
    (if (left.length > right.length) left.toCharArray.zip(("0".repeat(left.length - right.length) + right).toCharArray) else right.toCharArray.zip(("0".repeat(right.length - left.length) + left).toCharArray))
      .count { case (cl, cr) => cl != cr }
  }

  def hammingDistance2(x: Int, y: Int): Int = {
    Integer.bitCount(x ^ y)
  }

  def main(args: Array[String]): Unit = {
    println(hammingDistance1(1, 4) == 2)
    println(hammingDistance1(3, 1) == 1)
    println(hammingDistance2(1, 4) == 2)
    println(hammingDistance2(3, 1) == 1)
  }

}
