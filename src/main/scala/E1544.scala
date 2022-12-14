object E1544 {

  import scala.collection.mutable

  def makeGood(s: String): String = {
    val res = mutable.ListBuffer[Char]()
    for (ch <- s) {
      if (res.isEmpty) res.append(ch)
      else if (res.last.toLower == ch.toLower && (res.last.isUpper ^ ch.isUpper)) res.dropRightInPlace(1)
      else res.append(ch)
    }
    res.mkString("")
  }

  def main(args: Array[String]): Unit = {
    println(makeGood("leEeetcode") == "leetcode")
    println(makeGood("abBAcC") == "")
    println(makeGood("s") == "s")
  }

}
