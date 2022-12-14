object BW92E4 {

  import scala.collection.mutable

  val MOD = 1000000007

  def countPalindromes(s: String): Int = {
    val m: mutable.Map[Char, mutable.ArrayBuffer[Int]] = mutable.HashMap[Char, mutable.ArrayBuffer[Int]]()
    for ((ch, i) <- s.zipWithIndex) {
      if (m.contains(ch)) m(ch).append(i)
      else m(ch) = mutable.ArrayBuffer(i)
    }
    var sum = 0
    for ((ch, l) <- m) {
      if (l.size > 1) {
        for (i <- 0 until l.size - 1) {
          for (j <- i + 1 until l.size) {
            if (l(j) - l(i) > 2) {
              sum = (sum + findNumOfTriplets(m, l(i), l(j))) % MOD
            }
          }
        }
      }
    }
    sum
  }

  private def findNumOfTriplets(m: mutable.Map[Char, mutable.ArrayBuffer[Int]], lo: Int, hi: Int): Int = {
    var sum = 0
    for ((_, a) <- m) {
      val l = a.filter(el => el > lo && el < hi)
      if (l.size > 1) {
        for (i <- 0 until l.size - 1) {
          for (j <- i + 1 until l.size) {
            sum = (sum + l(j) - l(i) - 1) % MOD
          }
        }
      }
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    println(countPalindromes("103301") == 2)
    println(countPalindromes("0000000") == 21)
    println(countPalindromes("9999900000") == 2)
  }

}
