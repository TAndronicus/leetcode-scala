object E492 {
  def constructRectangle(area: Int): Array[Int] = {
    for (i <- (2 until math.sqrt(area).toInt + 1).reverse) {
      val l = area / i
      if (l * i == area) return Array(l, i)
    }
    Array(area, 1)
  }

  def main(args: Array[String]): Unit = {
    println(constructRectangle(4) sameElements Array(2, 2))
    println(constructRectangle(37) sameElements Array(37, 1))
    println(constructRectangle(122122) sameElements Array(427, 286))
  }

}
