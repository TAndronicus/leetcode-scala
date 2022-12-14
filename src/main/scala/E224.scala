object E224 {
  sealed trait Eval {
    def evaluate(): Int
  }

  case class Add(left: Eval, right: Eval) extends Eval {
    override def evaluate(): Int = left.evaluate() + right.evaluate()
  }

  case class Unary(el: Eval) extends Eval {
    override def evaluate(): Int = -el.evaluate()
  }

  case class Literal(i: Int) extends Eval {
    override def evaluate(): Int = i
  }

  def calculate(s: String): Int = {
    evaluate(s, false)._1.evaluate()
  }

  private def evaluate(s: String, isNegated: Boolean): (Eval, String) =
    s.head match {
      case ' ' => evaluate(s.substring(1), isNegated)
      case '-' => evaluate(s.substring(1), true)
      case '(' =>
        val (left, remainderWithOperator) = evaluate(s.substring(1), false)
        val (operator, remainder) = extractOperator(remainderWithOperator)
        operator match {
          case ' ' =>
            (if (isNegated) Unary(left) else left, remainder)
          case '+' =>
            val (right, outOfScope) = evaluate(remainder, false)
            (if (isNegated) Add(Unary(left), right) else Add(left, right), outOfScope)
          case '-' =>
            val (right, outOfScope) = evaluate(remainder, true)
            (if (isNegated) Add(Unary(left), right) else Add(left, right), outOfScope)
        }
      case l if l.isDigit =>
        val (literal, remainderWithOperator) = evaluateLiteral(s.substring(1), l.asDigit, isNegated)
        val (operator, remainder) = extractOperator(remainderWithOperator)
        operator match {
          case ' ' =>
            (literal, remainder)
          case '+' =>
            val (right, outOfScope) = evaluate(remainder, false)
            (Add(literal, right), outOfScope)
          case '-' =>
            val (right, outOfScope) = evaluate(remainder, true)
            (Add(literal, right), outOfScope)
        }
    }

  private def extractOperator(s: String): (Char, String) =
    if (s.isEmpty) (' ', s)
    else s.head match {
      case ' ' => extractOperator(s.substring(1))
      case ')' => (' ', s.substring(1))
      case c => (c, s.substring(1))
    }

  private def evaluateLiteral(s: String, currentValue: Int, isNegated: Boolean): (Eval, String) =
    if (s.isEmpty) (if (isNegated) Unary(Literal(currentValue)) else Literal(currentValue), s)
    else s.head match {
      case l if l.isDigit => evaluateLiteral(s.substring(1), currentValue * 10 + l.asDigit, isNegated)
      case _ => (if (isNegated) Unary(Literal(currentValue)) else Literal(currentValue), s)
    }


  def main(args: Array[String]): Unit = {
    println(calculate("1+2+3 +  4") == 10)
    println(calculate("1+2+3 -  4") == 2)
    println(calculate(" -  1+2+3 -  4") == 0)
    println(calculate("-(1+2)+3-4") == -4)
    println(calculate("-(1+2)+3-4-4") == -8)
    println(calculate("-(1+2)+3-(4-4) ") == 0)
    println(calculate("-(1+2)+3-((4-4)-2)") == 2)
    println(calculate("-(1+2)+3-((4-4)-2)+((1))") == 3)
    println(calculate("1 + 1") == 2)
    println(calculate(" 2-1 + 2 ") == 3)
    println(calculate("(1+(4+5+2)-3)+(6+8)") == 23)
    println(calculate(" 2-1 + 25 ") == 26)
  }

}
