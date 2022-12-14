object E295 {

  import scala.collection.mutable

  private class MedianFinderSortedSet() {

    private val m = mutable.SortedMap[Int, Int]()
    private var median = 0
    private var position = 0d
    private var sizeMod = 0

    def addNum(num: Int) {
      m.updateWith(num) {
        case Some(value) => Some(value + 1)
        case None => Some(1)
      }
      sizeMod = (sizeMod + 1) % 2
      if (m.size == 1) {
        median = num
        position = 1
      } else updateMedian(num)
    }

    private def updateMedian(num: Int): Unit = {
      if (num > median) {
        if (position >= m(median) && sizeMod == 1) {
          median = m.minAfter(median + 1).map(_._1).get
          position = 1
        } else position += .5
      } else if (num < median) {
        if (position == 1) {
          val newMedian = m.maxBefore(median).get
          median = newMedian._1
          position = newMedian._2 + (if (sizeMod == 0) .5 else 0)
        } else position -= .5
      } else position += .5
    }

    def findMedian(): Double = {
      if (sizeMod == 0 && position > m(median)) {
        (m.minAfter(median + 1).map(_._1).get + median) / 2.0
      } else median
    }

  }

  class MedianFinder() {

    private val lowerStack = new mutable.PriorityQueue[Int]()(Ordering.Int)
    private val upperStack = new mutable.PriorityQueue[Int]()(Ordering.Int.reverse)
    private var even = false
    private var median = 0d

    def addNum(num: Int) {
      if (lowerStack.isEmpty && upperStack.isEmpty) {
        lowerStack.enqueue(num)
        median = num
      } else {
        enqueue(num)
        balanceStacks()
        updateMedian()
      }
    }

    private def enqueue(num: Int): Unit = {
      if (num <= median) lowerStack.enqueue(num)
      else upperStack.enqueue(num)
      even = !even
    }

    private def balanceStacks(): Unit = {
      if (upperStack.size > lowerStack.size) lowerStack.enqueue(upperStack.dequeue())
      else if (lowerStack.size - upperStack.size > 1) upperStack.enqueue(lowerStack.dequeue())
    }

    private def updateMedian(): Unit = {
      if (even) median = (lowerStack.head + upperStack.head) / 2.0
      else median = lowerStack.head
    }

    def findMedian(): Double = {
      median
    }

  }

  def main(args: Array[String]): Unit = {
    println(testMedianFinder(Array(1, 1, 1, 1, 1, 1), Array(1, 1, 1, 1, 1, 1)))
    println(testMedianFinder(Array(1, 2, 1, 2, 1), Array(1, 1.5, 1, 1.5, 1)))
    println(testMedianFinder(Array(1, 2, 3, 4, 5), Array(1, 1.5, 2, 2.5, 3)))
    println(testMedianFinder(Array(1, 2, 3, 4, 5, 1, 1, 1), Array(1, 1.5, 2, 2.5, 3, 2.5, 2, 1.5)))
    println(testMedianFinder(Array(1, 1, 2, 1, 0, 1, 2, 2, 0, 0, 1), Array(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))
    //    val mf = new MedianFinder()
    //    mf.addNum(1)
    //    mf.addNum(2)
    //    println(mf.findMedian())
  }

  private def testMedianFinderSortedSet(nums: Array[Int], medians: Array[Double]): Boolean = {
    val mf = new MedianFinderSortedSet()
    for ((num, median) <- nums.zip(medians)) {
      mf.addNum(num)
      //      println(mf.findMedian())
      if (mf.findMedian() != median) return false
    }
    true
  }

  private def testMedianFinder(nums: Array[Int], medians: Array[Double]): Boolean = {
    val mf = new MedianFinder()
    for ((num, median) <- nums.zip(medians)) {
      mf.addNum(num)
      //      println(mf.findMedian())
      if (mf.findMedian() != median) return false
    }
    true
  }

}
