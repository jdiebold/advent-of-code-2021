val input = io.Source
  .fromFile("src/main/scala/Day19/test.txt")
  .mkString
  .split("--- scanner \\d+ ---\\n")
  .toSeq
  .tail
  .map(_.split("\\n").toSeq.map(_.split(",").map(_.toInt).toSeq))

val _0 = input(0)
val _1 = input(1)

def calcDistances(signals: Seq[Seq[Int]]) =
  for {
    pair <- signals.combinations(2).toSeq
    dist = pair(0).zip(pair(1)).map(x => (x._1 - x._2).abs).sorted
  } yield (pair, dist)

def calcOverlap(scanner1: Seq[Seq[Int]], scanner2: Seq[Seq[Int]]) =
  calcDistances(scanner2)
    .filter(d => calcDistances(scanner1).map(_._2).contains(d._2))
    .flatMap(_._1)
    .distinct
    .size

input
  .combinations(2)
  .filter(p => calcOverlap(p(0), p(1)) == 12)
  .toSeq
