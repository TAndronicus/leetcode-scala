object E500 {
  private val characterMap = Map(
    'q' -> 0,
    'w' -> 0,
    'e' -> 0,
    'r' -> 0,
    't' -> 0,
    'y' -> 0,
    'u' -> 0,
    'i' -> 0,
    'o' -> 0,
    'p' -> 0,
    'a' -> 1,
    's' -> 1,
    'd' -> 1,
    'f' -> 1,
    'g' -> 1,
    'h' -> 1,
    'j' -> 1,
    'k' -> 1,
    'l' -> 1,
    'z' -> 2,
    'x' -> 2,
    'c' -> 2,
    'v' -> 2,
    'b' -> 2,
    'n' -> 2,
    'm' -> 2
  )

  def findWords(words: Array[String]): Array[String] = {
    words.filter(word => word.length == 1 || singleRow(word))
  }

  private def singleRow(word: String): Boolean = {
    val rowNum = characterMap(word(0).toLower)
    for (ch <- word) if (characterMap(ch.toLower) != rowNum) return false
    true
  }

  def main(args: Array[String]): Unit = {
    println(findWords(Array("Hello", "Alaska", "Dad", "Peace")) sameElements Array("Alaska", "Dad"))
    println(findWords(Array("omk")) sameElements Array[String]())
    println(findWords(Array("adsdf", "sfd")) sameElements Array("adsdf", "sfd"))
  }

}
