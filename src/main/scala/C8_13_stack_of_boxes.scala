object C8_13_stack_of_boxes {

  import scala.collection.mutable

  private def strictlyLess(stack: (Int, Int, Int, Int), box: (Int, Int, Int)): Boolean = {
    stack._1 < box._1 && stack._2 < box._2 && stack._3 < box._3
  }

  def maxStackOfBoxesHeight(boxes: Array[(Int, Int, Int)]): Int = {
    var maxHeight = 0
    val stacks = mutable.ListBuffer[(Int, Int, Int, Int)]()
    for (box <- boxes.sortBy(_._2)) {
      val newStacks = mutable.ListBuffer[(Int, Int, Int, Int)]((box._1, box._2, box._3, box._2))
      if (box._2 > maxHeight) maxHeight = box._2
      for (stack <- stacks if strictlyLess(stack, box)) {
        newStacks.append((box._1, box._2, box._3, box._2 + stack._4))
        if (box._2 + stack._4 > maxHeight) maxHeight = box._2 + stack._4
      }
      stacks.addAll(newStacks)
      newStacks.clear()
    }
    maxHeight
  }

  def main(args: Array[String]): Unit = {
    println(maxStackOfBoxesHeight(Array((1, 1, 1), (2, 2, 2), (3, 3, 3))) == 6)
    println(maxStackOfBoxesHeight(Array((1, 1, 1), (2, 2, 1), (1, 3, 3))) == 3)
    println(maxStackOfBoxesHeight(Array((1, 1, 1), (2, 2, 1), (1, 3, 3), (3, 4, 5))) == 7)
  }

}
