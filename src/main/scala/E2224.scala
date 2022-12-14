object E2224 {
  private val diff = Array(60, 15, 5, 1)

  def convertTime(current: String, correct: String): Int = {
    var start = current.split(":")(0).toInt * 60 + current.split(":")(1).toInt
    val end = correct.split(":")(0).toInt * 60 + correct.split(":")(1).toInt
    var counter = 0
    var i = 0
    while (start != end) {
      while (end - start >= diff(i)) {
        start += diff(i); counter += 1
      }
      i += 1
    }
    counter
  }

  def main(args: Array[String]): Unit = {
    println(convertTime("02:30", "04:35") == 3)
    println(convertTime("11:00", "11:01") == 1)
  }

}
