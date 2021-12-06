val fish = io.Source
  .fromFile("src/main/scala/Day06/test.txt")
  .getLines
  .next
  .split(",")
  .map(_.toInt)
  .toList

//Part I
def spawn(fish: List[Int]): List[Int] =
  fish.flatMap(f => if (f == 0) List(6, 8) else List(f - 1))

val f = Function.chain(List.fill(80)(spawn))
f(fish).size

//Part II
val fishCounts =
  fish.groupBy(identity).map((k, v) => k -> v.size.toLong)
fishCounts.get(3).getOrElse(0L) + fishCounts.get(0).getOrElse(0L)

def spawnII(counts: Map[Int, Long]): Map[Int, Long] =
  Map(
    0 -> counts.get(1).getOrElse(0L),
    1 -> counts.get(2).getOrElse(0L),
    2 -> counts.get(3).getOrElse(0L),
    3 -> counts.get(4).getOrElse(0L),
    4 -> counts.get(5).getOrElse(0L),
    5 -> counts.get(6).getOrElse(0L),
    6 -> counts.get(7).getOrElse(0L).`+`(counts.get(0).getOrElse(0L)),
    7 -> counts.get(8).getOrElse(0L),
    8 -> counts.get(0).getOrElse(0L)
  )

def spawnIII(counts: Map[Int, Long]): Map[Int, Long] =
  counts
    .flatMap((k, v) => {
      k match {
        case 0 =>
          Map(
            8 -> v,
            6 -> counts.get(7).getOrElse(0L).`+`(counts.get(0).getOrElse(0L))
          )
        case _ => Map(k - 1 -> v)
      }
    })
    .updated(6, counts.get(7).getOrElse(0L) + counts.get(0).getOrElse(0L))

val fII = Function.chain(List.fill(256)(spawnIII))
fII(fishCounts).foldLeft(0L)(_ + _._2) == 26984457539L
