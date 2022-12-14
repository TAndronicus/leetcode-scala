object E520 {
  def detectCapitalUse(word: String): Boolean = {
    (word.length == 1) ||
      (word.head.isLower && allCase(word.substring(1), false)) ||
      (word.head.isUpper && allCase(word.substring(2), word(1).isUpper))
  }

  private def allCase(word: String, upper: Boolean): Boolean = {
    for (c <- word) {
      if (c.isUpper ^ upper) return false
    }
    true
  }


  def main(args: Array[String]): Unit = {
    println(!detectCapitalUse("mL"))
    println(detectCapitalUse("USD"))
    println(detectCapitalUse("Us"))
    println(detectCapitalUse("Usd"))
    println(detectCapitalUse("usd"))
    println(!detectCapitalUse("usD"))
  }

}
