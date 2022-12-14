import scala.collection.mutable.ListBuffer

object E990 {

  import scala.collection.mutable

  def equationsPossible(equations: Array[String]): Boolean = {
    val equalMap = mutable.Map[Char, Char]()
    val inequalities = ListBuffer[String]()
    for (equation <- equations) {
      if (equation(1) == '!') {
        if (equation(0) == equation(3)) return false
        else inequalities.addOne(equation)
      } else {
        val (s, l) = (equation(0), equation(3))
        if (equalMap.contains(s)) {
          if (equalMap.contains(l)) {
            if (!sameLoop(equalMap, s, l)) {
              var pointer = equalMap(s)
              while (equalMap(pointer) != s) pointer = equalMap(pointer)
              equalMap(pointer) = l
              pointer = equalMap(l)
              while (equalMap(pointer) != l) pointer = equalMap(pointer)
              equalMap(pointer) = s
            }
          } else {
            equalMap.put(l, equalMap(s))
            equalMap(s) = l
          }
        } else {
          if (equalMap.contains(l)) {
            equalMap.put(s, equalMap(l))
            equalMap(l) = s
          } else {
            equalMap.put(s, l)
            equalMap.put(l, s)
          }
        }
      }
    }
    for (inequality <- inequalities) {
      val (s, l) = (inequality(0), inequality(3))
      if (equalMap.contains(s) && equalMap.contains(l) && sameLoop(equalMap, s, l)) return false
    }
    true
  }

  private def sameLoop(map: mutable.Map[Char, Char], first: Char, second: Char): Boolean = {
    // check for existence?
    var pointer = map(first)
    while (second != pointer && first != pointer) pointer = map(pointer)
    pointer == second
  }

  def main(args: Array[String]): Unit = {
    println(!equationsPossible(Array("a==b", "b!=a")))
    println(equationsPossible(Array("b==a", "a==b")))
    println(!equationsPossible(Array("b==a", "c==d", "c==a", "d!=a")))
    println(!equationsPossible(Array("b==a", "c==d", "c==a", "e!=e")))
  }

}
