object C8_14_bool_evaluation {

  def evaluateBoolExpression(s: String): Int = {
    evaluateBoolExpression(s, true)
  }

  private def evaluateBoolExpression(exp: String, b: Boolean): Int = {
    val firstBit = exp(0) == '1'
    if (exp.length == 1) if (b ^ !firstBit) 1 else 0
    else {
      val (operator, secondBit) = (exp(1), exp(2) == '1')
      val reduceFirst = operator match {
        case '&' => firstBit & secondBit
        case '|' => firstBit | secondBit
        case '^' => firstBit ^ secondBit
      }
      evaluateBoolExpression((if (reduceFirst) '1' else '0') + exp.substring(3), b) + operator match {
        case '&' => if (firstBit) evaluateBoolExpression(exp.substring(2), true) else 0
        case '|' => evaluateBoolExpression(exp.substring(2), true) + (if (firstBit) evaluateBoolExpression(exp.substring(2), false) else 0)
        case '^' => evaluateBoolExpression(exp.substring(2), !firstBit)
        case _ => {
          println("co jest kurwa " + operator)
          0
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    //    println(evaluateBoolExpression("1") == 1)
    //    println(evaluateBoolExpression("0") == 0)
    //    println(evaluateBoolExpression("0|0&1") == 0)
    println(evaluateBoolExpression("1|0&1") == 2)
    //    println(evaluateBoolExpression("1|0&1") == 2)
  }

}
