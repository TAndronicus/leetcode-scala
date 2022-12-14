
object E345 {
  def reverseVowels(s: String): String = {
    if (s.length < 2) return s
    val vowels = Set('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')
    val array = s.toCharArray
    var start = -1
    var end = s.length
    while (true) {
      do {
        start += 1
      } while (start < s.length && !vowels.contains(s.charAt(start)))
      do {
        end -= 1
      } while (end > 0 && !vowels.contains(s.charAt(end)))
      if (start >= end) return array.mkString
      swap(array, start, end)
    }
    ""
  }

  private def swap(array: Array[Char], start: Int, end: Int): Unit = {
    val tmp = array(start)
    array(start) = array(end)
    array(end) = tmp
  }

  def main(args: Array[String]): Unit = {
    println(reverseVowels("hello") == "holle")
    println(reverseVowels("h") == "h")
    println(reverseVowels("he") == "he")
    println(reverseVowels("ee") == "ee")
    println(reverseVowels("ea") == "ae")
    println(reverseVowels("Aa") == "aA")
    println(reverseVowels("") == "")
  }

}
