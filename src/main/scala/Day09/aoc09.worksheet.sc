import scala.language.postfixOps
val cave = io.Source
  .fromFile("src/main/scala/Day09/input.txt")
  .getLines
  .map(_.split("").map(_.toInt).toList)
  .toList

val dirs = List((-1, 0), (0, -1), (1, 0), (0, 1))
def lowest(cave: List[List[Int]], y: Int, x: Int): Boolean =
  dirs
    .map(pos =>
      cave(y)(x) < cave
        .lift(y + pos._1)
        .getOrElse(List(9))
        .lift(x + pos._2)
        .getOrElse(9)
    )
    .reduce(_ && _)

// Part I
(0 until cave.length)
  .flatMap { y =>
    (0 until cave(0).length)
      .collect {
        case x if lowest(cave, y, x) => cave(y)(x)
      }
  }
  .map(_ + 1)
  .sum

// Part II
def getBasinSize(
    cave: List[List[Int]],
    y: Int,
    x: Int
): Set[(Int, Int)] =
  if (cave.lift(y).getOrElse(List(9)).lift(x).getOrElse(9) == 9) Set()
  else
    dirs
      .map(pos =>
        if (
          cave
            .lift(y + pos._1)
            .getOrElse(List(9))
            .lift(x + pos._2)
            .getOrElse(9) > cave(y)(x)
        ) getBasinSize(cave, y + pos._1, x + pos._2)
        else Set()
      )
      .foldLeft(Set((x, y)))(_ ++ _)

(0 to cave.size - 1)
  .flatMap { y =>
    (0 to cave(0).size - 1)
      .collect { case x if lowest(cave, y, x) => getBasinSize(cave, y, x).size }
  }
  .sorted
  .reverse
  .take(3)
  .product
