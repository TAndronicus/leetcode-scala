object E151 {
  def reverseWords(s: String): String = {
    s.split(" ")
      .filter(_.nonEmpty)
      .reverse
      .mkString(" ")
  }

  def main(args: Array[String]): Unit = {
    println(reverseWords("the sky is blue") == "blue is sky the")
    println(reverseWords("  hello world  ") == "world hello")
    println(reverseWords("a good   example") == "example good a")
  }

}
