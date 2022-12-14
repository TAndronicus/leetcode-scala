object E83 {
  def deleteDuplicates(head: ListNode): ListNode = {
    val res = new ListNode()
    var (lastItem, originalPointer, resultPointer) = (-101, head, res)
    while (originalPointer != null) {
      if (originalPointer.x != lastItem) {
        lastItem = originalPointer.x
        resultPointer.next = new ListNode(_x = lastItem)
        resultPointer = resultPointer.next
      }
      originalPointer = originalPointer.next
    }
    res.next
  }


  def main(args: Array[String]): Unit = {
    println(E61.fromListNode(deleteDuplicates(null)) sameElements Array[Int]())
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1)))) sameElements Array(1))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 2, 2, 3)))) sameElements Array(1, 2, 3))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 1, 1)))) sameElements Array(1))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 2, 3, 3, 4, 4, 5)))) sameElements Array(1, 2, 3, 4, 5))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 1, 2, 3)))) sameElements Array(1, 2, 3))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 2)))) sameElements Array(1, 2))
    println(E61.fromListNode(deleteDuplicates(E61.fromArray(Array(1, 1, 2, 3, 3)))) sameElements Array(1, 2, 3))
  }

}
