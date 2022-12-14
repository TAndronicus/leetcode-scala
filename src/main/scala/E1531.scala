object E1531 {

  import scala.collection.mutable

  def getLengthOfOptimalCompression(s: String, k: Int): Int = {
    if (k == s.length) return 0
    if (k == s.length - 1) return 1
    val m = new mutable.TreeMap[Int, Int]()(Ordering.by[Int, Int](i => i - math.pow(10, math.log10(i).toInt).toInt))
    var (curLen, curCh) = (1, s(0))
    for (ch <- s.toCharArray.tail) {
      if (ch == curCh) curLen += 1
      else {
        m.put(curLen, m.getOrElse(curLen, 0) + 1)
        curLen = 1
        curCh = ch
      }
    }
    m.put(curLen, m.getOrElse(curLen, 0) + 1)

    var left = k
    while (left > 0) {
      val (len, _) = m.min
      val newLen = math.log10(len) match {
        case l if l == len.toDouble =>
          left -= 1
          len - 1
        case pow =>
          val cost = math.max(1, math.min(len - math.pow(10, pow.toInt).toInt, k))
          left -= cost
          len - cost
      }
      m.updateWith(len)(_.map(_ - 1).filter(_ > 0))
      if (newLen > 0) m.updateWith(newLen) {
        case Some(c) => Some(c + 1)
        case None => Some(1)
      }
    }

    m.map { case (len, cnt) => (math.ceil(math.log10(if (len == 1) 1 else len + 1)).toInt + 1) * cnt }
      .foldLeft(0) { case (acc, next) => acc + next }
  }


  def main(args: Array[String]): Unit = {
    /*println(getLengthOfOptimalCompression("aaabcccd", 2) == 4)
    println(getLengthOfOptimalCompression("aabbaa", 2) == 2)
    println(getLengthOfOptimalCompression("aaaaaaaaaaa", 0) == 3)*/

  }

}
