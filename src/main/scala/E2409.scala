object E2409 {
  private val cumDays = Map(
    1 -> 0,
    2 -> 31,
    3 -> 59,
    4 -> 90,
    5 -> 120,
    6 -> 151,
    7 -> 181,
    8 -> 212,
    9 -> 243,
    10 -> 273,
    11 -> 304,
    12 -> 334
  )

  def countDaysTogether(arriveAlice: String, leaveAlice: String, arriveBob: String, leaveBob: String): Int = {
    val (maxArrive, minLeave) = ((if (arriveAlice > arriveBob) arriveAlice else arriveBob).split("-").map(_.toInt), (if (leaveAlice > leaveBob) leaveBob else leaveAlice).split("-").map(_.toInt))
    math.max(cumDays(minLeave(0)) + minLeave(1) + 1 - cumDays(maxArrive(0)) - maxArrive(1), 0)
  }

  def main(args: Array[String]): Unit = {
    println(countDaysTogether("08-15", "08-18", "08-16", "08-19") == 3)
    println(countDaysTogether(arriveAlice = "10-01", leaveAlice = "10-31", arriveBob = "11-01", leaveBob = "12-31") == 0)
    println(countDaysTogether(arriveAlice = "01-01", leaveAlice = "01-31", arriveBob = "11-01", leaveBob = "12-31") == 0)
  }

}
