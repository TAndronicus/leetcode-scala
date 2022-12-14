

object E278 {
  def firstBadVersion(n: Int, isBadVersion: Int => Boolean): Int = {
    if (n == 1) 1
    else if (n == 2) if (isBadVersion(1)) 1 else 2
    else {
      var index = if (n == 2147483647) n / 2 else (n + 1) / 2
      var jump = if (n == 2147483647) n / 2 else (n + 1) / 2
      while (jump > 1) {
        jump = (jump + 1) / 2
        if (isBadVersion(index)) index -= jump
        else index += jump
      }
      if (index > 1 && isBadVersion(index - 1)) index - 1
      else if (index >= 1 && isBadVersion(index)) index
      else index + 1
    }
  }


  def main(args: Array[String]): Unit = {
    println(firstBadVersion(1, i => i >= 1) == 1)
    println(firstBadVersion(2, i => i >= 1) == 1)
    println(firstBadVersion(2, i => i > 1) == 2)
    println(firstBadVersion(3, i => i > 1) == 2)
    println(firstBadVersion(4, i => i > 1) == 2)
    println(firstBadVersion(5, i => i > 1) == 2)
    println(firstBadVersion(6, i => i > 1) == 2)
    println(firstBadVersion(7, i => i > 1) == 2)
    println(firstBadVersion(8, i => i > 1) == 2)
    println(firstBadVersion(9, i => i > 1) == 2)
    println(firstBadVersion(186327, i => i > 100) == 101)
    println(firstBadVersion(2147483647, i => i >= 2147483647) == 2147483647)
  }

}
