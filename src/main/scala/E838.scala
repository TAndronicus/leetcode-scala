object E838 {
  def pushDominoes(dominoes: String): String = {
    val rightDist = new Array[Int](dominoes.length)
    val res = new Array[Char](dominoes.length)
    for (i <- dominoes.indices) {
      dominoes(i) match {
        case 'R' => rightDist(i) = 0
        case 'L' => rightDist(i) = Int.MaxValue
        case _ => rightDist(i) = if (i == 0 || rightDist(i - 1) == Int.MaxValue) Int.MaxValue else rightDist(i - 1) + 1
      }
    }
    var leftDist = Int.MaxValue
    for (i <- dominoes.indices.reverse) {
      dominoes(i) match {
        case 'L' =>
          res(i) = 'L'
          leftDist = 0
        case 'R' =>
          res(i) = 'R'
          leftDist = Int.MaxValue
        case _ =>
          leftDist = if (leftDist == Int.MaxValue) Int.MaxValue else leftDist + 1
          res(i) = if (leftDist < rightDist(i)) 'L' else if (leftDist > rightDist(i)) 'R' else '.'
      }
    }
    res.mkString
  }

  def main(args: Array[String]): Unit = {
    println(pushDominoes("RR.L") == "RR.L")
    println(pushDominoes(".L.R...LR..L..") == "LL.RR.LLRRLL..")
    println(pushDominoes("...L") == "LLLL")
    println(pushDominoes("...R") == "...R")
    println(pushDominoes("...R.L") == "...R.L")
  }

}
