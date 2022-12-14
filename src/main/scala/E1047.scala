object E1047 {

  import scala.collection.mutable

  def removeDuplicates(s: String): String = {
    val l = mutable.ListBuffer[Char]()
    for (ch <- s) if (l.nonEmpty && l.last == ch) l.dropRightInPlace(1) else l.append(ch)
    l.mkString("")
  }

  def main(args: Array[String]): Unit = {
    println(removeDuplicates("abbaca") == "ca")
    println(removeDuplicates("azxxzy") == "ay")
  }

}
