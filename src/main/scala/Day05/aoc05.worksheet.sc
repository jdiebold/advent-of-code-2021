val data = io.Source.fromFile("src/main/scala/Day05/input.txt").getLines.toList
val lines =
  data.map(_.split(" -> ").map(_.split(",").map(_.toInt).toList).toList).toList

def drawLine(lineDef: List[List[Int]]): List[List[Int]] =
  val diffs = lineDef(0).zip(lineDef(1)).map(t => t._2 - t._1)
  val div: List[Int] = diffs.map(n => n * n.sign)
  val diff = diffs.map(_ / div.max)
  List(lineDef(0)) ++ (1 to div.max)
    .map(m => lineDef(0).zip(diff.map(_ * m)).map(t => t._1 + t._2))
    .toList

// part II
val linesPart1 = lines.filter(p => p(0)(0) == p(1)(0) || p(0)(1) == p(1)(1))
linesPart1
  .flatMap(drawLine(_))
  .groupBy(identity)
  .map((a, b) => a -> b.size)
  .values
  .filter(_ > 1)
  .size

// part II
lines
  .flatMap(drawLine)
  .groupBy(identity)
  .map((a, b) => a -> b.size)
  .values
  .filter(_ > 1)
  .size
