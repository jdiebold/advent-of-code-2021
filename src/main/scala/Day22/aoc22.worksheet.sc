val input = io.Source
  .fromFile("src/main/scala/Day22/input.txt")
  .getLines
  .toSeq

val stepsI = input.map(parseLine)

def parseLine(line: String): (Boolean, (Int, Int), (Int, Int), (Int, Int)) =
  val matches = """-*\d+\.\.-*\d+""".r.findAllMatchIn(line).toSeq
  val x = matches(0).matched.split("\\.\\.").map(_.toInt)
  val y = matches(1).matched.split("\\.\\.").map(_.toInt)
  val z = matches(2).matched.split("\\.\\.").map(_.toInt)
  (line.startsWith("on"), (x(0), x(1)), (y(0), y(1)), (z(0), z(1)))

//Part I
def applyStep(
    dots: Set[(Int, Int, Int)],
    step: (Boolean, (Int, Int), (Int, Int), (Int, Int))
): Set[(Int, Int, Int)] =
  val changed = for {
    x <- ((step._2._1 max -50) to (step._2._2 min 50))
    y <- ((step._3._1 max -50) to (step._3._2 min 50))
    z <- ((step._4._1 max -50) to (step._4._2 min 50))
  } yield (x, y, z)
  if (step._1) dots ++ changed
  else
    dots -- changed

stepsI.foldLeft(Set[(Int, Int, Int)]())(applyStep(_, _)).size

//Part II
case class Cuboid(x: (Int, Int), y: (Int, Int), z: (Int, Int)) {
  def +(other: Cuboid): Set[Cuboid] =
    Set(other) ++ (this - other)

  def -(other: Cuboid): Set[Cuboid] =
    if (
      (this.x._1 > other.x._2) || (this.x._2 < other.x._1) ||
      (this.y._1 > other.y._2) || (this.y._2 < other.y._1) ||
      (this.z._1 > other.z._2) || (this.z._2 < other.z._1)
    ) Set(this)
    else
      // 6 possible new cuboids may emerge
      Set(
        // x axis
        Option.when(other.x._1 > this.x._1)(
          Cuboid((this.x._1, other.x._1 - 1), this.y, this.z)
        ),
        Option.when(other.x._2 < this.x._2)(
          Cuboid((other.x._2 + 1, this.x._2), this.y, this.z)
        ),
        // y axis
        Option.when(other.y._1 > this.y._1)(
          Cuboid(
            (other.x._1 max this.x._1, other.x._2 min this.x._2),
            (this.y._1, other.y._1 - 1),
            this.z
          )
        ),
        Option.when(other.y._2 < this.y._2)(
          Cuboid(
            (other.x._1 max this.x._1, other.x._2 min this.x._2),
            (other.y._2 + 1, this.y._2),
            this.z
          )
        ),
        // z axis
        Option.when(other.z._1 > this.z._1)(
          Cuboid(
            (other.x._1 max this.x._1, other.x._2 min this.x._2),
            (other.y._1 max this.y._1, other.y._2 min this.y._2),
            (this.z._1, other.z._1 - 1)
          )
        ),
        Option.when(other.z._2 < this.z._2)(
          Cuboid(
            (other.x._1 max this.x._1, other.x._2 min this.x._2),
            (other.y._1 max this.y._1, other.y._2 min this.y._2),
            (other.z._2 + 1, this.z._2)
          )
        )
      ).flatMap(identity)

  def volume: BigInt =
    BigInt(
      this.x._2 - this.x._1 + 1
    ) * BigInt(this.y._2 - this.y._1 + 1) * BigInt(this.z._2 - this.z._1 + 1)

  override def toString: String =
    s"Cuboid(x=$x, y=$y, z=$z)"

  override def equals(that: Any): Boolean =
    that match {
      case that: Cuboid =>
        this.x == that.x && this.y == that.y && this.z == that.z
      case _ => false
    }
}

val stepsII = stepsI.map(s => (s._1, Cuboid(s._2, s._3, s._4)))

def applyStepII(dots: Set[Cuboid], step: (Boolean, Cuboid)): Set[Cuboid] =
  if (step._1)
    dots.flatMap(_ + step._2)
  else
    dots.flatMap(_ - step._2)

val res = stepsII.tail
  .foldLeft(Set(stepsII.head._2))(applyStepII(_, _))
  .toSeq
  .map(_.volume)
  .sum
