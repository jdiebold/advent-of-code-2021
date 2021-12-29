val input = io.Source
  .fromFile("src/main/scala/Day25/input.txt")
  .getLines
  .toSeq
  .map(_.split("").toSeq)

val hSize = input(0).size
val vSize = input.size

val east = (for {
  hPos <- (0 until hSize)
  vPos <- (0 until vSize)
  if input(vPos)(hPos) == ">"
} yield (vPos, hPos)).toSeq

val south = (for {
  hPos <- (0 until hSize)
  vPos <- (0 until vSize)
  if input(vPos)(hPos) == "v"
} yield (vPos, hPos))

def step(
    east: IndexedSeq[(Int, Int)],
    south: IndexedSeq[(Int, Int)],
    vSize: Int,
    hSize: Int
): (IndexedSeq[(Int, Int)], IndexedSeq[(Int, Int)]) =
  // move east first
  val newEast = east.map(x =>
    if (!(east ++ south).contains((x._1, (x._2 + 1) % hSize)))
      (x._1, (x._2 + 1) % hSize)
    else x
  )
  // then move south
  val newSouth = south.map(x =>
    if (!(newEast ++ south).contains(((x._1 + 1) % vSize, x._2)))
      ((x._1 + 1) % vSize, x._2)
    else x
  )
  (newEast, newSouth)

val (east1, south1) = step(east, south, vSize, hSize)
printState(east, south, vSize, hSize)
printState(east1, south1, vSize, hSize)

def printState(
    east: IndexedSeq[(Int, Int)],
    south: IndexedSeq[(Int, Int)],
    vSize: Int,
    hSize: Int
): Unit =
  (0 until vSize).map(y =>
    println(
      (0 until hSize)
        .map(x =>
          if (east.contains((y, x))) ">"
          else if (south.contains((y, x))) "v"
          else "."
        )
        .mkString
    )
  )

val res = Iterator
  .iterate((east, south))(s => step(s._1, s._2, vSize, hSize))
  .sliding(2)
  .takeWhile(i => i(0) != i(1))
  .toSeq
  .size + 1
