import scala.collection.mutable.ArrayBuffer

//class ListNode(_x: Int = 0, _next: ListNode = null) {
//  var next: ListNode = _next
//  var x: Int = _x
//}

object E86 {
  def partition(head: ListNode, x: Int): ListNode = {
    val (lo, hi) = (new ListNode(), new ListNode())
    var (loPointer, hiPointer, pointer) = (lo, hi, head)
    while (pointer != null) {
      if (pointer.x < x) {
        loPointer.next = new ListNode(_x = pointer.x)
        loPointer = loPointer.next
      } else {
        hiPointer.next = new ListNode(_x = pointer.x)
        hiPointer = hiPointer.next
      }
      pointer = pointer.next
    }
    loPointer.next = hi.next
    lo.next
  }

  def main(args: Array[String]): Unit = {
    println(fromListNode(partition(fromArray(Array(1, 4, 3, 2, 5, 2)), 3)) sameElements Array(1, 2, 2, 4, 3, 5))
    println(fromListNode(partition(fromArray(Array(2, 1)), 2)) sameElements Array(1, 2))
    println(fromListNode(partition(fromArray(Array(1, 1)), 2)) sameElements Array(1, 1))
    println(fromListNode(partition(fromArray(Array(1, 2, 3)), 2)) sameElements Array(1, 2, 3))
    println(fromListNode(partition(fromArray(Array(1, 3, 2)), 2)) sameElements Array(1, 3, 2))
    println(fromListNode(partition(fromArray(Array(1, 3, 2, 1)), 2)) sameElements Array(1, 1, 3, 2))
    println(fromListNode(partition(null, 2)) sameElements Array[Int]())
  }

  def fromArray(ar: Array[Int]): ListNode = {
    val listNode = new ListNode(_x = ar(0))
    var pointer = listNode
    for (el <- ar.tail) {
      pointer.next = new ListNode(_x = el)
      pointer = pointer.next
    }
    return listNode
  }

  def fromListNode(ln: ListNode): Array[Int] = {
    val ab = ArrayBuffer[Int]()
    var pointer = ln
    while (pointer != null) {
      ab += pointer.x
      pointer = pointer.next
    }
    return ab.toArray
  }

}
