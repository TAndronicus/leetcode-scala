
object E658 {

  import scala.collection.mutable

  def findClosestElements(arr: Array[Int], k: Int, x: Int): List[Int] = {
    if (arr.length == k) return arr.toList
    if (arr.last <= x) return arr.takeRight(k).toList
    if (arr.head >= x) return arr.take(k).toList

    if (k < arr.length / 2) findMinority(arr, k, x)
    else findMajority(arr, k, x)
  }

  private def findMinority(arr: Array[Int], k: Int, x: Int): List[Int] = {
    var left = arr.length / 2
    var jump = left
    while (!(arr(left) <= x && arr(left + 1) >= x)) {
      jump = (jump + 1) / 2
      if (arr(left) > x) left -= jump
      else left += jump
    }
    var right = left + 1
    val res = mutable.ListBuffer[Int]()
    while (res.size < k) {
      if (left < 0 || (right < arr.length && x - arr(left) > arr(right) - x)) {
        res.append(arr(right))
        right += 1
      } else {
        res.prepend(arr(left))
        left -= 1
      }
    }
    res.toList
  }

  private def findMajority(arr: Array[Int], k: Int, x: Int): List[Int] = {
    var (left, right) = (0, arr.length - 1)
    while (right - left > k - 1) {
      if (x - arr(left) > arr(right) - x) left += 1 else right -= 1
    }
    arr.slice(left, left + k).toList
  }

  def main(args: Array[String]): Unit = {
    println(findClosestElements(Array(1, 2, 3, 4, 5), 4, 3) == List(1, 2, 3, 4))
    println(findClosestElements(Array(1, 2, 3, 4, 5), 4, -1) == List(1, 2, 3, 4))
    println(findClosestElements(Array(1, 2, 3, 4, 5), 2, 4) == List(3, 4))
    println(findClosestElements(Array(1, 2, 3, 4, 5), 2, 5) == List(4, 5))
    println(findClosestElements(Array(1, 2, 3, 4, 5), 2, 6) == List(4, 5))
    println(findClosestElements(Array(1, 2, 3, 4, 5), 5, 6) == List(1, 2, 3, 4, 5))
    println(findClosestElements(Array(1, 1, 1, 10, 10, 10), 1, 9) == List(10))
    println(findClosestElements(Array(1, 1, 1, 1, 1, 10), 1, 9) == List(10))
    println(findClosestElements(Array(0, 1, 2, 3, 4, 4, 4, 5, 5, 5, 6, 7, 9, 9, 10, 10, 11, 11, 12, 13, 14, 14, 15, 17, 19, 19, 22, 24, 24, 25, 25, 27, 27, 29, 30, 32, 32, 33, 33, 35, 36, 38, 39, 41, 42, 43, 44, 44, 46, 47, 48, 49, 52, 53, 53, 54, 54, 57, 57, 58, 59, 59, 59, 60, 60, 60, 61, 61, 62, 64, 66, 68, 68, 70, 72, 72, 74, 74, 74, 75, 76, 76, 77, 77, 80, 80, 82, 83, 85, 86, 87, 87, 92, 93, 94, 96, 96, 97, 98, 99), 25, 90))
  }

}
