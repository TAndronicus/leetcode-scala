object E82 {
  def deleteDuplicates(head: ListNode): ListNode = {
    val res = new ListNode()
    var (lastItem, matched, originalPointer, resultPointer) = (-101, true, head, res)
    while (originalPointer != null) {
      if (originalPointer.x == lastItem) {
        matched = true
      } else {
        if (matched) {
          matched = false
        } else {
          resultPointer.next = new ListNode(_x = lastItem)
          resultPointer = resultPointer.next
        }
        lastItem = originalPointer.x
      }
      originalPointer = originalPointer.next
    }
    if (!matched) resultPointer.next = new ListNode(_x = lastItem)
    res.next
  }


  def main(args: Array[String]): Unit = {
    println(E61.fromListNode(deleteDuplicates(null)) sameElements Array[Int]())
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1)))) sameElements Array(1))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 2, 2, 3)))) sameElements Array(1, 3))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 1, 1)))) sameElements Array[Int]())
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 2, 3, 3, 4, 4, 5)))) sameElements Array(1, 2, 5))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 1, 2, 3)))) sameElements Array(2, 3))
  }

}
