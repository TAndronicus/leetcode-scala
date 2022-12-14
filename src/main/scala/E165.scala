object E165 {
  def compareVersion(version1: String, version2: String): Int = {
    val (a1, a2) = (version1.split("\\.").map(_.toInt), version2.split("\\.").map(_.toInt))
    for (i <- 0 until math.max(a1.length, a2.length)) {
      if (getOrZero(a1, i) < getOrZero(a2, i)) return -1
      if (getOrZero(a1, i) > getOrZero(a2, i)) return 1
    }
    0
  }

  private def getOrZero(a: Array[Int], index: Int): Int = {
    if (index >= a.length) 0
    else a(index)
  }

  def main(args: Array[String]): Unit = {
    println(compareVersion("1.01", "1.001") == 0)
    println(compareVersion("1.01", "1.0011") == -1)
    println(compareVersion("1.012", "1.0011") == 1)
    println(compareVersion("1.9", "9.1") == -1)
    println(compareVersion("1.0.1", "1") == 1)
    println(compareVersion("1.0.0", "1.0") == 0)
  }

}
