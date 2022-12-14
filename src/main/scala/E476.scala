object E476 {
  def findComplement(num: Int): Int = {
    Integer.valueOf(Integer.toString(num, 2).map(ch => if (ch == '0') '1' else '0'), 2)
  }

  def main(args: Array[String]): Unit = {
    println(findComplement(5) == 2)
    println(findComplement(1) == 0)
    println(findComplement(7) == 0)
    println(findComplement(10) == 5)
  }

}
