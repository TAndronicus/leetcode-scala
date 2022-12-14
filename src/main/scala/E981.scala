object E981 {

  import scala.collection.mutable

  class TimeMap() {
    val map = mutable.HashMap[String, mutable.TreeMap[Int, String]]()

    def set(key: String, value: String, timestamp: Int) {
      if (map.contains(key)) map(key).put(timestamp, value)
      else map.put(key, mutable.TreeMap(timestamp -> value))
    }

    def get(key: String, timestamp: Int): String = {
      map.get(key)
        .flatMap(tm => if (tm.contains(timestamp)) tm.get(timestamp) else tm.maxBefore(timestamp).map(_._2))
        .getOrElse("")
    }

  }

  def main(args: Array[String]): Unit = {
    val timeMap = new TimeMap()
    timeMap.set("foo", "bar", 1)
    println(timeMap.get("foo", 1) == "bar")
    println(timeMap.get("foo", 3) == "bar")
    timeMap.set("foo", "bar2", 4)
    println(timeMap.get("foo", 4) == "bar2")
    println(timeMap.get("foo", 5) == "bar2")
    println(timeMap.get("foo", 0) == "")
  }

}
