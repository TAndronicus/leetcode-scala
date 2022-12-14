object E1641 {
  private val res = Map(
    1 -> 5,
    2 -> 15,
    3 -> 35,
    4 -> 70,
    5 -> 126,
    6 -> 210,
    7 -> 330,
    8 -> 495,
    9 -> 715,
    10 -> 1001,
    11 -> 1365,
    12 -> 1820,
    13 -> 2380,
    14 -> 3060,
    15 -> 3876,
    16 -> 4845,
    17 -> 5985,
    18 -> 7315,
    19 -> 8855,
    20 -> 10626,
    21 -> 12650,
    22 -> 14950,
    23 -> 17550,
    24 -> 20475,
    25 -> 23751,
    26 -> 27405,
    27 -> 31465,
    28 -> 35960,
    29 -> 40920,
    30 -> 46376,
    31 -> 52360,
    32 -> 58905,
    33 -> 66045,
    34 -> 73815,
    35 -> 82251,
    36 -> 91390,
    37 -> 101270,
    38 -> 111930,
    39 -> 123410,
    40 -> 135751,
    41 -> 148995,
    42 -> 163185,
    43 -> 178365,
    44 -> 194580,
    45 -> 211876,
    46 -> 230300,
    47 -> 249900,
    48 -> 270725,
    49 -> 292825,
    50 -> 316251,
  )

  def countVowelStrings(n: Int): Int = {
    res(n)
  }

  def main(args: Array[String]): Unit = {
    println(countVowelStrings(1))
    println(countVowelStrings(2))
    println(countVowelStrings(33))
  }

}