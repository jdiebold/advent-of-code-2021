import $dep.`org.scalanlp::breeze:2.0.1-RC1`
import $dep.`org.scala-lang.modules::scala-parallel-collections:1.0.4`
import breeze.linalg._
import breeze.numerics._
import scala.collection.parallel.CollectionConverters._

val sensors = io.Source
  .fromFile("src/main/scala/Day19/input.txt")
  .mkString
  .split("--- scanner \\d+ ---\\n")
  .toSeq
  .tail
  .map(_.split("\\n").toSeq.map(_.split(",").map(_.toInt).toSeq))
  .map(_.map(v => DenseVector(v.toArray)))
  .par

def calcDistances(
    sensor: Seq[DenseVector[Int]]
): Seq[(DenseVector[Int], Seq[DenseVector[Int]])] =
  for {
    beacon <- sensor
    dist = sensor.map(_ - beacon)
  } yield (beacon, dist)

val rotations = Seq(
  // turn around y
  DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
  DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
  DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
  DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
  // turn around x
  DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
  DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
  DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
  DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
  // turn around z
  DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1)),
  DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1)),
  DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1)),
  DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1))
)

val permutations = rotations.toSet ++
  rotations
    .combinations(2)
    .flatMap(_.permutations)
    .map(_.reduceLeft(_ * _))
    .toSet ++
  rotations
    .combinations(3)
    .flatMap(_.permutations)
    .map(_.reduceLeft(_ * _))
    .toSet ++
  rotations
    .combinations(4)
    .flatMap(_.permutations)
    .map(_.reduceLeft(_ * _))
    .toSet

def normalizeBeacons(
    left: Seq[DenseVector[Int]],
    right: Seq[DenseVector[Int]]
): Option[(Seq[DenseVector[Int]], DenseVector[Int])] =
  val matchingBeacons = for {
    left <- calcDistances(left)
    right <- calcDistances(right)
    perm <- permutations
    count = left._2.intersect(right._2.map(perm * _)).size
    if (count >= 12)
  } yield (left._1, right._1, perm)
  matchingBeacons
    .lift(0)
    .map(b =>
      val rightSensorPosition =
        b._1 - b._3 * b._2
      (
        (left ++ (right.map(
          b._3 * _ + rightSensorPosition
        ))).distinct,
        rightSensorPosition
      )
    )

val (beacons, rest, normalizedSensors) = Iterator
  .iterate((sensors.head, sensors.tail, Seq(DenseVector(0, 0, 0))))(i =>
    val res = for {
      other <- i._2
      newPoints = normalizeBeacons(i._1, other)
      if (newPoints.isDefined)
    } yield (newPoints.get, other)
    (res(0)._1._1, i._2.filter(_ != res(0)._2), i._3 :+ res(0)._1._2)
  )
  .find(_._2.isEmpty)
  .get

//Part I
beacons.distinct.size

//Part II
normalizedSensors
  .combinations(2)
  .map(sensorPair => manhattanDistance(sensorPair(0), sensorPair(1)))
  .toSeq
  .max
  .toInt
