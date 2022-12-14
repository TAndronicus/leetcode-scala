import scala.collection.mutable

class StockSpanner() {

  private val largerStack = mutable.ArrayDeque[(Int, Int)]()
  private var counter = 0

  def next(price: Int): Int = {
    while (largerStack.nonEmpty && largerStack.last._1 <= price) largerStack.removeLast()
    val previousLargest = largerStack.lastOption.map(_._2).getOrElse(0)
    counter += 1
    largerStack.append((price, counter))
    counter - previousLargest
  }

}

object E901 {

  def main(args: Array[String]): Unit = {
    val stockSpanner = new StockSpanner()
    println(stockSpanner.next(100) == 1)
    println(stockSpanner.next(80) == 1)
    println(stockSpanner.next(60) == 1)
    println(stockSpanner.next(70) == 2)
    println(stockSpanner.next(60) == 1)
    println(stockSpanner.next(75) == 4)
    println(stockSpanner.next(85) == 6)
  }

}
