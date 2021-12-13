import scala.collection.immutable.ArraySeq
val input = io.Source
  .fromFile("src/main/scala/Day13/input.txt")
  .getLines
  .map(_.split(",").toSeq)
  .toSeq

val dots = input.splitAt(input.indexWhere(_.length == 1))(0).map(_.map(_.toInt))
val folds = input.splitAt(input.indexWhere(_.length == 1))(1).tail.flatten

def fold(dots: Seq[Seq[Int]], fold: String): Seq[Seq[Int]] =
  val foldDir = "[x,y]".r.findFirstIn(fold).get
  val foldPos = """\d+""".r.findFirstIn(fold).get.toInt
  dots.map(dot => {
    if (foldDir == "x")
      if (dot(0) > foldPos) Seq(foldPos - (dot(0) - foldPos), dot(1))
      else dot
    else if (dot(1) > foldPos) Seq(dot(0), foldPos - (dot(1) - foldPos))
    else dot
  })
//Part I
fold(dots, folds(0)).toSet.size
//Part II
val result = folds.foldLeft(dots)((b, a) => fold(b, a)).toSet

(0 to result.map(_.tail.head).max)
  .map(y =>
    (0 to result.map(_.head).max).map(x =>
      if (result.contains(Seq(x, y))) "#" else "."
    )
  )
  .map(_.mkString)
  .foreach(println)
