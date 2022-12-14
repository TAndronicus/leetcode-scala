object BW91E3 {

  import scala.collection.mutable

  class Tree(val id: Int, val amount: Int, val children: Set[Tree], var parent: Tree)

  def mostProfitablePath(edges: Array[Array[Int]], bob: Int, amount: Array[Int]): Int = {
    val m = mutable.HashMap[Int, mutable.Set[Int]]()
    for (edge <- edges) addToMap(m, edge(0), edge(1))
    val t = buildTree(m, 0, mutable.Set[Int](), amount)
    val b = findBob(t, bob)
    val bobPrize = buildBobMap(b)
    val q = mutable.ArrayDeque((t, 0, 0))
    var maxPrize = Int.MinValue
    while (q.nonEmpty) {
      val (nextTree, counter, amount) = q.removeHead()
      val prize = if (bobPrize.contains(nextTree.id) && bobPrize(nextTree.id) <= counter) {
        amount + (if (bobPrize(nextTree.id) == counter) nextTree.amount / 2 else 0)
      } else amount + nextTree.amount
      if (nextTree.children.isEmpty) {
        if (prize > maxPrize) maxPrize = prize
      } else q.addAll(nextTree.children.map((_, counter + 1, prize)))
    }
    maxPrize
  }

  private def buildBobMap(bob: Tree): mutable.Map[Int, Int] = {
    val q = mutable.ArrayDeque[Tree](bob)
    val res = mutable.HashMap[Int, Int]()
    var counter = 0
    while (q.nonEmpty) {
      val next = q.removeHead()
      res.put(next.id, counter)
      counter += 1
      if (next.parent != null) q.addOne(next.parent)
    }
    res
  }

  private def findBob(t: Tree, bob: Int): Tree = {
    val q = mutable.ArrayDeque[Tree](t)
    while (q.nonEmpty) {
      val next = q.removeHead()
      if (next.id == bob) return next
      q.addAll(next.children)
    }
    null
  }

  private def buildTree(m: mutable.HashMap[Int, mutable.Set[Int]], current: Int, visited: mutable.Set[Int], amount: Array[Int]): Tree = {
    visited.add(current)
    val t = new Tree(current, amount(current), m(current).filter(!visited.contains(_)).map(buildTree(m, _, visited, amount)).toSet, null)
    t.children.foreach(_.parent = t)
    t
  }

  private def addToMap(m: mutable.HashMap[Int, mutable.Set[Int]], a: Int, b: Int): Unit = {
    addToMapOneWay(m, a, b)
    addToMapOneWay(m, b, a)
  }

  private def addToMapOneWay(m: mutable.HashMap[Int, mutable.Set[Int]], a: Int, b: Int): Unit = {
    if (m.contains(a)) m(a).add(b) else m.put(a, mutable.Set(b))
  }

  def main(args: Array[String]): Unit = {
    println(mostProfitablePath(Array(Array(0, 1), Array(1, 2), Array(1, 3), Array(3, 4)), 3, Array(-2, 4, 2, -4, 6)))
    println(mostProfitablePath(Array(Array(0, 1)), 1, Array(-7280, 2350)))
    println(mostProfitablePath(Array(Array(0, 1), Array(1, 2), Array(2, 3)), 3, Array(-5644, -6018, 1188, -8502)))
  }

}
