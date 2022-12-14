object E1293 {

  import scala.collection.mutable

  def initializeDp(grid: Array[Array[Int]]): Array[Array[Int]] = {
    val (m, n) = (grid.length, grid(0).length)
    val dp = generateObstacleGrid(m, n)
    if (grid.head.head == 1) return dp
    else dp(0)(0) = 0
    val (visited, queue) = (generateVisitedSet, mutable.ArrayDeque[(Int, Int)]())
    enqueueSecondBlocks(m, n, queue)
    while (queue.nonEmpty) {
      val (i, j) = queue.removeHead()
      visited.add((i, j))
      if (grid(i)(j) != 1) {
        dp(i)(j) = findMinDist(i, j, m, n, dp)
        populateQueue(i, j, m, n, visited, queue)
      }
    }
    dp
  }

  def shortestPath(grid: Array[Array[Int]], k: Int): Int = {
    val (m, n) = (grid.length, grid(0).length)
    var prev = initializeDp(grid)
    var next = generateObstacleGrid(m, n)
    for (kk <- 0 until k) {
      val (visited, queue) = (generateVisitedSet, mutable.ArrayDeque[(Int, Int)]())
      next(0)(0) = 0
      enqueueSecondBlocks(m, n, queue)
      while (queue.nonEmpty) {
        val (i, j) = queue.removeHead()
        visited.add((i, j))
        val dist = findMinDist(i, j, m, n, if (prev(i)(j) > 1000) prev else next)
        if (dist < 1000) populateQueue(i, j, m, n, visited, queue)
        next(i)(j) = dist
      }
      prev = next
      next = generateObstacleGrid(m, n)
    }
    if (prev(m - 1)(n - 1) > 1000) -1 else prev(m - 1)(n - 1)
  }

  private def generateVisitedSet = {
    val s = mutable.Set[(Int, Int)]()
    s.add((0, 0))
    s
  }

  private def enqueueSecondBlocks(m: Int, n: Int, queue: mutable.ArrayDeque[(Int, Int)]) = {
    if (m > 1) queue.append((1, 0))
    if (n > 1) queue.append((0, 1))
  }

  private def generateObstacleGrid(m: Int, n: Int) = {
    Array.fill(m)(Array.fill(n)(Int.MaxValue - 1))
  }

  private def findMinDist(i: Int, j: Int, m: Int, n: Int, grid: Array[Array[Int]]): Int = {
    var dist = Int.MaxValue - 1
    if (i > 0) dist = math.min(dist, grid(i - 1)(j) + 1)
    if (i < m - 1) dist = math.min(dist, grid(i + 1)(j) + 1)
    if (j > 0) dist = math.min(dist, grid(i)(j - 1) + 1)
    if (j < n - 1) dist = math.min(dist, grid(i)(j + 1) + 1)
    dist
  }

  private def populateQueue(i: Int, j: Int, m: Int, n: Int, visited: mutable.Set[(Int, Int)], queue: mutable.ArrayDeque[(Int, Int)]) = {
    if (i > 0 && !visited((i - 1, j))) queue.append((i - 1, j))
    if (i < m - 1 && !visited((i + 1, j))) queue.append((i + 1, j))
    if (j > 0 && !visited((i, j - 1))) queue.append((i, j - 1))
    if (j < n - 1 && !visited((i, j + 1))) queue.append((i, j + 1))
  }

  def main(args: Array[String]): Unit = {
    //    println(shortestPath(Array(Array(0, 0, 0), Array(1, 1, 0), Array(0, 0, 0), Array(0, 1, 1), Array(0, 0, 0)), 1) == 6)
    //    println(shortestPath(Array(Array(0, 0, 0), Array(1, 1, 0), Array(0, 0, 0), Array(0, 1, 1), Array(0, 0, 0)), 0) == 10)
    //    println(shortestPath(Array(Array(0, 1, 1), Array(1, 1, 1), Array(1, 0, 0)), 1) == -1)
    println(shortestPath(Array(Array(0, 1, 1), Array(1, 1, 1), Array(1, 0, 0)), 2) == 4)
    println(shortestPath(Array(Array(0, 1, 1), Array(1, 1, 1), Array(1, 0, 0)), 2))
  }

}
