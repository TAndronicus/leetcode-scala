object BW88E1 {
  def equalFrequency(word: String): Boolean = {
    val lengths = word.toCharArray
      .groupBy(ch => ch)
      .values
      .map(ar => ar.length)
      .toList
      .sorted
      .toArray
    if (lengths.length >= 3) (lengths(0) == lengths(lengths.length - 2) && lengths(lengths.length - 2) + 1 == lengths(lengths.length - 1)) || (lengths(0) == 1 && lengths(1) == lengths(lengths.length - 1))
    else (lengths.length == 2 && lengths(0) + 1 == lengths(1)) || lengths(0) == 1
  }

  def main(args: Array[String]): Unit = {
    println(equalFrequency("bac"))
    println(equalFrequency("abcc"))
    println(!equalFrequency("aazz"))
    println(equalFrequency("aazzz"))
    println(!equalFrequency("aazzzz"))
  }

}
