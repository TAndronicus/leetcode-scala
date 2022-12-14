object BW88E2 {
  class LUPrefix(_n: Int) {

    import scala.collection.mutable

    val set = mutable.SortedSet((1 until _n + 1): _*)

    def upload(video: Int) {
      set.remove(video)
    }

    def longest(): Int = {
      if (set.nonEmpty) set.firstKey - 1 else _n
    }

  }

  def main(args: Array[String]): Unit = {
    val lu = new LUPrefix(4)
    lu.upload(3)
    println(lu.longest() == 0)
    lu.upload(1)
    println(lu.longest() == 1)
    lu.upload(2)
    println(lu.longest() == 3)
  }

}
