object E212 {

  import scala.collection.mutable

  def findWords(board: Array[Array[Char]], words: Array[String]): List[String] = {
    val m = mutable.HashMap[Char, mutable.Set[String]]()
    for (word <- words) {
      if (m.contains(word.head)) m(word.head).add(word)
      else m(word.head) = mutable.Set(word)
    }
    val res = mutable.ListBuffer[String]()
    val visited = Array.fill(board.length)(Array.fill(board(0).length)(false))
    for (i <- board.indices) {
      for (j <- board(0).indices) {
        val candidates = m.getOrElse(board(i)(j), mutable.Set[String]())
        for (word <- candidates) {
          if (isPlausible(board, visited, i, j, word.tail)) {
            m(board(i)(j)).remove(word)
            res.append(word)
          }
        }
      }
    }
    res.toList
  }

  private def isPlausible(board: Array[Array[Char]], visited: Array[Array[Boolean]], i: Int, j: Int, toMatch: String): Boolean = {
    visited(i)(j) = true
    val matched = toMatch.isEmpty ||
      (i > 0 && board(i - 1)(j) == toMatch.head && !visited(i - 1)(j) && isPlausible(board, visited, i - 1, j, toMatch.tail)) ||
      (j > 0 && board(i)(j - 1) == toMatch.head && !visited(i)(j - 1) && isPlausible(board, visited, i, j - 1, toMatch.tail)) ||
      (i < board.length - 1 && board(i + 1)(j) == toMatch.head && !visited(i + 1)(j) && isPlausible(board, visited, i + 1, j, toMatch.tail)) ||
      (j < board(0).length - 1 && board(i)(j + 1) == toMatch.head && !visited(i)(j + 1) && isPlausible(board, visited, i, j + 1, toMatch.tail))
    visited(i)(j) = false
    matched
  }

  def main(args: Array[String]): Unit = {
    println(findWords(Array(Array('o', 'a', 'a', 'n'), Array('e', 't', 'a', 'e'), Array('i', 'h', 'k', 'r'), Array('i', 'f', 'l', 'v')), Array("oath", "pea", "eat", "rain")).mkString(","))
    println(findWords(Array(Array('a', 'b'), Array('c', 'd')), Array("abcb")).mkString(","))
  }

}
