
object E290 {

  import scala.collection.mutable

  def wordPattern(pattern: String, s: String): Boolean = {
    val words = s.split(" ")
    if (pattern.length != words.length) false
    else {
      val bijectionChar = mutable.Map[Char, String]()
      val bijectionWord = mutable.Map[String, Char]()
      for ((char, word) <- pattern.zip(words)) {
        if (bijectionChar.contains(char) && bijectionChar(char) != word) return false
        else if (bijectionWord.contains(word) && bijectionWord(word) != char) return false
        else bijectionChar(char) = word; bijectionWord(word) = char
      }
      true
    }
  }


  def main(args: Array[String]): Unit = {
    println(wordPattern("abba", "dog cat cat dog"))
    println(!wordPattern("abba", "dog cat cat fish"))
    println(!wordPattern("aaaa", "dog cat cat dog"))
    println(!wordPattern("abba", "dog dog dog dog"))
  }

}
