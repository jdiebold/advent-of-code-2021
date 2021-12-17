val input = io.Source
  .fromFile("src/main/scala/Day17/input.txt")
  .getLines
  .next
  .substring(13)
  .split(", ")
  .map(_.substring(2).split("\\.\\.").map(_.toInt).toSeq)
  .toSeq

def makeStep(pos: (Int, Int), vel: (Int, Int)): ((Int, Int), (Int, Int)) =
  (
    (pos._1 + vel._1, pos._2 + vel._2),
    (vel._1 + (vel._1.sign * -1), vel._2 - 1)
  )

def probeOnTrack(
    pos: (Int, Int),
    vel: (Int, Int),
    target: Seq[Seq[Int]]
): Boolean =
  if (
    (pos._1 > target(0).max && vel._1.sign > 0)
    || (pos._1 < target(0).min && vel._1.sign < 0)
    || (pos._2 < target(1).min && vel._2.sign < 0)
  ) false
  else
    true

def inTarget(pos: (Int, Int), target: Seq[Seq[Int]]) =
  (pos._1 <= target(0).max && pos._1 >= target(0).min && pos._2 >= target(
    1
  ).min && pos._2 <= target(1).max)

def shoot(
    start: (Int, Int),
    vel: (Int, Int),
    target: Seq[Seq[Int]]
): Option[List[(Int, Int)]] =
  val trajectory = Iterator
    .iterate(
      (start, vel)
    )(makeStep)
    .takeWhile(x => probeOnTrack(x._1, x._2, input))
    .toList
  if (inTarget(trajectory.last._1, input)) Some(trajectory.map(_._1))
  else None

// Part I
(Math.round(Math.sqrt(input(0).map(_.abs).min * 2)).toInt - 5 to Math
  .round(Math.sqrt(input(0).map(_.abs).max * +2))
  .toInt)
  .flatMap(vx => (0 to 1000).flatMap(vy => shoot((0, 0), (vx, vy), input)))
  .flatten
  .map(_._2)
  .max

// Part II
(Math.round(Math.sqrt(input(0).map(_.abs).min * 2)).toInt - 5 to input(0).max)
  .flatMap(vx =>
    (input(1).min to 1000).flatMap(vy => shoot((0, 0), (vx, vy), input))
  )
  .size
