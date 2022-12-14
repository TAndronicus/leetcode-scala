object E692 {

  import scala.collection.mutable

  def topKFrequent(words: Array[String], k: Int): List[String] = {
    val m = mutable.Map[String, Int]()
    for (word <- words) m.put(word, m.getOrElse(word, 0) + 1)
    m.toList
      .sortWith { case ((w1, c1), (w2, c2)) => (if (c1 != c2) c1 - c2 else w2.compareTo(w1)) > 0 }
      .take(k)
      .map(_._1)
  }


  def main(args: Array[String]): Unit = {
    println(topKFrequent(Array("i", "love", "leetcode", "i", "love", "coding"), 2) == List("i", "love"))
    println(topKFrequent(Array("the", "day", "is", "sunny", "the", "the", "the", "sunny", "is", "is"), 4) == List("the", "is", "sunny", "day"))
  }

}
