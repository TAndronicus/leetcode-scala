object E77 {
  def combine(n: Int, k: Int): List[List[Int]] =
    combine((1 to n).toList, List(List()), k)

  private def combine(numbersLeft: List[Int], acc: List[List[Int]], elementsLeft: Int): List[List[Int]] =
    if (elementsLeft == 0) acc
    else numbersLeft.indices.map(i => combine(numbersLeft.drop(i + 1), acc.map(numbersLeft(i) :: _), elementsLeft - 1)).foldLeft(List[List[Int]]())(_ ::: _)


  def main(args: Array[String]): Unit = {
    //    println(List(1, 2, 3, 4, 5, 6).drop(1))
    println(combine(1, 1) == List(List(1)))
    println(sameElements(combine(4, 2), List(List(1, 2), List(1, 3), List(1, 4), List(2, 3), List(2, 4), List(3, 4))))
    println(sameElements(combine(4, 4), List(List(1, 2, 3, 4))))
  }

  def sameElements(l1: List[List[Int]], l2: List[List[Int]]): Boolean = l1.size == l2.size && l1.map(_.sorted).forall(l2.map(_.sorted).contains)

}
