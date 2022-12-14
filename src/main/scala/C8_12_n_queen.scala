object C8_12_n_queen {

  import scala.collection.mutable

  def arrangeQueens(n: Int): Array[Array[(Int, Int)]] = {
    val res = mutable.ArrayBuffer[Array[(Int, Int)]]()
    arrangeQueens(n, mutable.ArrayDeque[(Int, Int)](), mutable.Set[Int](), mutable.Set[Int](), mutable.Set[Int](), mutable.Set[Int](), res)
    res.toArray
  }

  private def arrangeQueens(n: Int, path: mutable.ArrayDeque[(Int, Int)], r: mutable.Set[Int], c: mutable.Set[Int], rpc: mutable.Set[Int], rmc: mutable.Set[Int], res: mutable.ArrayBuffer[Array[(Int, Int)]]): Unit = {
    for (i <- (if (path.isEmpty) 0 else path.last._1) until n if !r.contains(i)) {
      r.add(i)
      for (j <- 0 until n if !c.contains(j)) {
        if (!(rpc.contains(i + j) || rmc.contains(i - j))) {
          c.add(j)
          if (r.size == n) {
            path.append((i, j))
            res.addOne(path.toArray)
            r.remove(i);
            c.remove(j);
            path.removeLast()
            return
          } else {
            rpc.add(i + j);
            rmc.add(i - j);
            path.append((i, j))
            arrangeQueens(n, path, r, c, rpc, rmc, res)
            rpc.remove(i + j);
            rmc.remove(i - j);
            path.removeLast()
          }
          c.remove(j)
        }
      }
      r.remove(i)
    }
  }

  def printChessboards(n: Int): Unit = {
    for (res <- arrangeQueens(n)) {
      for (i <- 0 until n) {
        //        println("-----------------")
        for (j <- 0 until n) {
          print("|" + (if (res.contains((i, j))) "*" else " "))
        }
        println("|")
      }
      //      println("-----------------")
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    printChessboards(8)
    val q = arrangeQueens(8)
    println(q.length + "\n")
    for (res <- q.take(10)) println(res.map { case (i, j) => s"($i, $j)" }.mkString("[", ", ", "]"))
    println()
    println()
    printChessboards(4)
    println()
    println()
    printChessboards(6)
  }

}
