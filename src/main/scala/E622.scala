object E622 {
  private class MyCircularQueue(_k: Int) {

    val ar = new Array[Int](_k)
    var head = 0
    var next = 0
    var full = false

    def enQueue(value: Int): Boolean = {
      if (isFull()) false
      else {
        ar(next) = value
        next = (next + 1) % _k
        full = head == next
        true
      }
    }

    def deQueue(): Boolean = {
      if (isEmpty()) false
      else {
        head = (head + 1) % _k
        full = false
        true
      }
    }

    def Front(): Int = {
      if (isEmpty()) -1 else ar(head)
    }

    def Rear(): Int = {
      if (isEmpty()) -1 else ar(if (next == 0) _k - 1 else next - 1)
    }

    def isEmpty(): Boolean = {
      !full && head == next
    }

    def isFull(): Boolean = {
      full
    }

  }


  def main(args: Array[String]): Unit = {
    val s = new MyCircularQueue(3)
    println(s.isEmpty())
    println(s.enQueue(1))
    println(!s.isEmpty())
    println(s.enQueue(2))
    println(s.enQueue(3))
    println(!s.enQueue(4))
    println(s.Rear() == 3)
    println(s.isFull())
    println(s.Front() == 1)
    println(s.deQueue())
    println(!s.isFull())
    println(s.enQueue(4))
    println(s.Rear() == 4)
    println(s.Front() == 2)
    println(s.deQueue())
    println(s.deQueue())
    println(s.deQueue())
    println(!s.deQueue())
    println(s.isEmpty())
    println(!s.isFull())
  }

}
