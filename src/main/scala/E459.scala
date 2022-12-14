object E459 {
  def repeatedSubstringPattern(s: String): Boolean = {
    val length = s.length
    for (i <- length / 2 to 1 by -1) {
      if (length % i == 0) {
        val pattern = s.substring(0, i)
        var matched = true
        for (j <- 1 until length / i) matched &&= (pattern == s.substring(j * i, (j + 1) * i))
        if (matched) return true
      }
    }
    false
  }

  def main(args: Array[String]): Unit = {
    println(repeatedSubstringPattern("aa"))
    println(repeatedSubstringPattern("aaa"))
    println(repeatedSubstringPattern("aaaa"))
    println(repeatedSubstringPattern("aaaaa"))
    println(repeatedSubstringPattern("abab"))
    println(repeatedSubstringPattern("ababab"))
    println(repeatedSubstringPattern("abaabaaba"))
    println(repeatedSubstringPattern("abaabacabaabac"))
    println(!repeatedSubstringPattern("a"))
    println(!repeatedSubstringPattern("ababa"))
    println(!repeatedSubstringPattern("abaabac"))
    println(!repeatedSubstringPattern("aaaaaaaaaaaaaac"))
    println(repeatedSubstringPattern("abcabcabcabc"))
    println(!repeatedSubstringPattern("aba"))
  }

}
