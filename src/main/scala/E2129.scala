object E2129 {
  def capitalizeTitle(title: String): String = {
    title.split(" ")
      .map(adaptWord)
      .mkString(sep = " ")
  }

  private def adaptWord(word: String): String = {
    if (word.length <= 2) word.toLowerCase
    else word.head.toUpper.toString + word.substring(1).toLowerCase
  }


  def main(args: Array[String]): Unit = {
    println(capitalizeTitle("capiTalIze tHe titLe") == "Capitalize The Title")
    println(capitalizeTitle("First leTTeR of EACH Word") == "First Letter of Each Word")
    println(capitalizeTitle("i lOve leetcode") == "i Love Leetcode")
  }

}
