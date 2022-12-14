

object E374 {
  def guessNumber(n: Int, guess: Int => Int): Int = {
    var jump = if (n == 2147483647) n / 2 else (n + 1) / 2
    var currVal = jump
    while (true) {
      jump = (jump + 1) / 2
      guess(currVal) match {
        case 0 => return currVal
        case -1 => currVal -= jump
        case 1 => currVal += jump
      }
    }
    -1
  }


  def main(args: Array[String]): Unit = {
    guessNumber(10, i => if (i < 10) 1 else if (i == 10) 0 else -1)
  }

}
