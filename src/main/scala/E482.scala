import scala.collection.mutable

object E482 {
  def licenseKeyFormatting(s: String, k: Int): String = {
    var left = k
    val firstIndex = math.min(k, s.indexOf('-'))
    val res = new mutable.StringBuilder(s.substring(0, firstIndex).toUpperCase())
    res.append('-')
    for (ch <- s.substring(firstIndex)) {
      if (left == 0) {
        res.append('-'); left = k
      }
      if (ch != '-') {
        res.append(ch.toUpper); left -= 1
      }
    }
    res.toString()
  }

  def main(args: Array[String]): Unit = {
    println(licenseKeyFormatting("2-5g-3-J", 2) == "2-5G-3J")
    println(licenseKeyFormatting("2222-5g-3-J", 2) == "22-22-5G-3J")
    //    println(licenseKeyFormatting("2-4A0r7-4k", 4) == "24A0-R74K")
  }

}
