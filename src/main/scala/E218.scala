object E218 {

  import scala.collection.mutable

  def getSkyline(buildings: Array[Array[Int]]): List[List[Int]] = {
    val heights = buildings.toList
      .flatMap(building => List(Array(building(0), building(2), building(1)), Array(building(1), -building(2), building(1))))
      .sorted(Ordering.by[Array[Int], (Int, Int)](ar => (ar(0), -ar(1))))
    var currentQueue = mutable.PriorityQueue.empty[Array[Int]](Ordering.by(_(2)))
    var newQueue = currentQueue
    var previousHeight = 0
    var previousPlace = -1
    val res = mutable.ListBuffer[List[Int]]()
    for (Array(place, height, reach) <- heights) {
      var wider = true
      var maxHeight = 0
      while (currentQueue.nonEmpty && wider) {
        val building = currentQueue.dequeue()
        if (building(2) > place) {
          maxHeight = math.max(maxHeight, building(1))
          newQueue.addOne(building)
        } else wider = false
      }
      currentQueue = newQueue
      newQueue = mutable.PriorityQueue.empty[Array[Int]](Ordering.by(_(2)))
      if (height > 0) {
        if (place != previousPlace && height > maxHeight && height != previousHeight) {
          res.append(List(place, height)); previousHeight = height; previousPlace = place
        }
        currentQueue.addOne(Array(place, height, reach))
      } else {
        if (place != previousPlace && maxHeight != previousHeight) {
          res.append(List(place, maxHeight)); previousHeight = maxHeight; previousPlace = place
        }
      }
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {
    println(getSkyline(Array(Array(2, 9, 10), Array(3, 7, 15), Array(5, 12, 12), Array(15, 20, 10), Array(19, 24, 8))))
    println(getSkyline(Array(Array(0, 2, 3), Array(2, 5, 3))))
    println(getSkyline(Array(Array(0, 2, 3), Array(2, 5, 2), Array(5, 6, 2))) == List(List(0, 3), List(2, 2), List(6, 0)))
    println(getSkyline(Array(Array(0, 3, 3), Array(1, 4, 2))) == List(List(0, 3), List(3, 2), List(4, 0)))
    println(getSkyline(Array(Array(0, 4, 3), Array(1, 3, 2))) == List(List(0, 3), List(4, 0)))
    println(getSkyline(Array(Array(0, 4, 2), Array(1, 3, 3))) == List(List(0, 2), List(1, 3), List(3, 2), List(4, 0)))
    println(getSkyline(Array(Array(0, 4, 2), Array(4, 6, 3))) == List(List(0, 2), List(4, 3), List(6, 0)))
    println(getSkyline(Array(Array(0, 4, 5), Array(4, 6, 3))) == List(List(0, 5), List(4, 3), List(6, 0)))
  }

}
