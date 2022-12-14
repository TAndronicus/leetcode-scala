import scala.collection.mutable.ArrayBuffer

//case class ListNode(_x: Int = 0, _next: ListNode = null) {
//  var next: ListNode = _next
//  var x: Int = _x
//}

object E61 {
  def rotateRight(head: ListNode, k: Int): ListNode = {
    if (head == null) return null
    if (k == 0) return head
    var (leading, lagging) = (head, head)
    var dist = 0
    while (leading.next != null) {
      leading = leading.next
      if (dist == k) lagging = lagging.next else dist += 1
    }
    if (dist != k) {
      val remainder = k % (dist + 1)
      if (remainder == 0) return head
      leading = head
      lagging = head
      dist = 0
      while (leading.next != null) {
        leading = leading.next
        if (dist == remainder) lagging = lagging.next else dist += 1
      }
    }
    val newHead = lagging.next
    lagging.next = null
    leading.next = head
    return newHead
  }

  def main(args: Array[String]): Unit = {
    println(rotateRight(null, 1) == null)
    println(fromListNode(rotateRight(fromArray(Array(1, 2, 3, 4)), 1)) sameElements Array(4, 1, 2, 3))
    println(fromListNode(rotateRight(fromArray(Array(1, 2, 3, 4)), 5)) sameElements Array(4, 1, 2, 3))
    println(fromListNode(rotateRight(fromArray(Array(1, 2, 3, 4)), 4)) sameElements Array(1, 2, 3, 4))
    println(fromListNode(rotateRight(fromArray(Array(1, 2, 3, 4, 5)), 2)) sameElements Array(4, 5, 1, 2, 3))
    println(fromListNode(rotateRight(fromArray(Array(0, 1, 2)), 4)) sameElements Array(2, 0, 1))
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
