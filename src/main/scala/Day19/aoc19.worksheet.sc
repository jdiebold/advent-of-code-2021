import $dep.`org.scalanlp::breeze:2.0.1-RC1`
import breeze.linalg._
import breeze.numerics._

val input = io.Source
  .fromFile("src/main/scala/Day19/test.txt")
  .mkString
  .split("--- scanner \\d+ ---\\n")
  .toSeq
  .tail
  .map(_.split("\\n").toSeq.map(_.split(",").map(_.toInt).toSeq))
  .map(_.map(v => DenseVector(v.toArray)))

val vector = DenseVector(1, 2, 3)
val matrix = DenseMatrix((0, 0, 1), (1, 0, 0), (0, 1, 0))
val vector2 = DenseVector(1, 2, 3)

matrix * vector

def calcDistances(
    sensor: Seq[DenseVector[Int]]
): Seq[(DenseVector[Int], Seq[DenseVector[Int]])] =
  for {
    beacon <- sensor
    dist = input(0).map(_ - beacon)
  } yield (beacon, dist)

val rotations = Seq(
  // turn around y
  DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
  DenseMatrix((-1, 0, 0), (0, 1, 0), (0, 0, -1)),
  DenseMatrix((0, 0, -1), (0, 1, 0), (1, 0, 0)),
  // turn around x
  DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
  DenseMatrix((1, 0, 0), (0, -1, 0), (0, 0, -1)),
  DenseMatrix((1, 0, 0), (0, 1, 0), (0, -1, 0)),
  // turn around z
  DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1)),
  DenseMatrix((-1, 0, 0), (-1, 0, 0), (0, 0, 1)),
  DenseMatrix((0, -1, 0), (0, 1, 0), (0, 0, 1))
)
