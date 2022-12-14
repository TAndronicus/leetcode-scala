object E1335 {

  def minDifficulty1(jobDifficulty: Array[Int], d: Int): Int = {
    val l = jobDifficulty.length
    if (d > l) return -1
    if (d == l) return jobDifficulty.sum
    if (d == 1) return jobDifficulty.max
    val dp = Array.fill(d - 1) {
      Array.fill(l)(0)
    }
    for (i <- 0 until l - 1) {
      for (j <- 0 until (d - 1)) {
        for (k <- i + 1 until l) {

        }
      }
    }
    0
  }


  def main(args: Array[String]): Unit = {
    /*println(getLengthOfOptimalCompression("aaabcccd", 2) == 4)
    println(getLengthOfOptimalCompression("aabbaa", 2) == 2)
    println(getLengthOfOptimalCompression("aaaaaaaaaaa", 0) == 3)*/

  }

}
