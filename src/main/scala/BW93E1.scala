object BW93E1 {
  def maximumValue(strs: Array[String]): Int = {
    var max: Int = 0
    for (st <- strs) {
      max = math.max(max, st.toIntOption.getOrElse(st.length))
    }
    max
  }

  def main(args: Array[String]): Unit = {
    println(maximumValue(Array("alic3", "bob", "3", "4", "00000")) == 5)
    println(maximumValue(Array("1", "01", "001", "0001")) == 1)
    println(maximumValue(Array("1", "01", "001", "0006")) == 6)
  }

}
