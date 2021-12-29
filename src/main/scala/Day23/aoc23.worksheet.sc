import scala.collection.immutable.SortedMap
import scala.collection.mutable.PriorityQueue
import scala.language.postfixOps
import $dep.`org.scala-lang.modules::scala-parallel-collections:1.0.4`

import scala.collection.parallel.CollectionConverters._

val input = io.Source
  .fromFile("src/main/scala/Day23/input.txt")
  .getLines
  .toSeq
  .map(_.split("").toSeq)

val target = io.Source
  .fromFile("src/main/scala/Day23/test2.txt")
  .getLines
  .toSeq
  .map(_.split("").toSeq)

val destinationRooms = Map("A" -> 3, "B" -> 5, "C" -> 7, "D" -> 9)

def canMoveOut(field: Seq[Seq[String]], pos: (Int, Int)): Boolean =
  pos._1 == 1 || ((pos._1 - 1 to 1 by -1)
    .map(field(_)(pos._2) == ".")
    .reduce(_ && _) &&
    (pos._2 != destinationRooms(field(pos._1)(pos._2)) ||
      !(2 to 5)
        .map(y => Seq(".", field(pos._1)(pos._2)).contains(field(y)(pos._2)))
        .reduce(_ && _)))

def possibleStepsOnHallway(
    field: Seq[Seq[String]],
    pos: (Int, Int)
): Seq[(Int, Int)] = {
  if (pos._1 > 1 && canMoveOut(field, pos))
    (field(1).lastIndexWhere(_ != ".", pos._2 - 1) + 1 until field(1)
      .indexWhere(
        _ != ".",
        pos._2 + 1
      )).filter(!Seq(3, 5, 7, 9).contains(_)).map((1, _)) diff Seq(pos)
  else
    Seq()
}

def possibleDestinationRoom(
    field: Seq[Seq[String]],
    pos: (Int, Int)
): Option[(Int, Int)] = {
  val destX = destinationRooms(field(pos._1)(pos._2))
  if (
    (canMoveOut(field, pos) &&
      destX < field(1).indexWhere(_ != ".", pos._2 + 1) &&
      destX > field(1).lastIndexWhere(_ != ".", pos._2 - 1)) &&
    field(2)(destX) == "." &&
    (2 to 5)
      .map(y => Seq(".", field(pos._1)(pos._2)).contains(field(y)(destX)))
      .reduce(_ && _)
  ) Some(((2 to 5).find(field(_)(destX) != ".").getOrElse(6) - 1, destX))
  else None
}

def makeMove(
    from: (Int, Int),
    to: (Int, Int),
    field: Seq[Seq[String]]
): Seq[Seq[String]] = {
  val amphipod = field(from._1)(from._2)
  val intfield = field
    .updated(from._1, field(from._1).updated(from._2, "."))
  intfield.updated(to._1, intfield(to._1).updated(to._2, amphipod))
}
def energyMove(
    from: (Int, Int),
    to: (Int, Int),
    field: Seq[Seq[String]]
): Int = {
  val steps = from._1 - 1 + to._1 - 1 + (from._2 - to._2).abs
  Math.pow(10, field(from._1)(from._2).head.toInt - 65).toInt * steps
}
val maxValue = Int.MaxValue
var minValue = maxValue

def next(
    field: Seq[Seq[String]],
    costs: Int
): Int = {
  if (costs > minValue) maxValue
  else if (
    field(2)(3) == "A" && field(3)(3) == "A" &&
    field(4)(3) == "A" && field(4)(3) == "A" &&
    field(2)(5) == "B" && field(3)(5) == "B" &&
    field(4)(5) == "B" && field(5)(5) == "B" &&
    field(2)(7) == "C" && field(3)(7) == "C" &&
    field(4)(7) == "C" && field(5)(7) == "C" &&
    field(2)(9) == "D" && field(3)(9) == "D" &&
    field(4)(9) == "D" && field(5)(9) == "D"
  ) costs
  else
    getNeighbors(field)
      .filter(_._2 + costs < minValue)
      .to(LazyList)
      .sortBy(i => (i._2))
      .map(i => next(i._1, i._2 + costs))
      .minOption
      .getOrElse(maxValue)
}

next(input, 0)
//44169
getNeighbors(input)
  .filter(_._2 + 0 < minValue)
  .toSeq
  .sortBy { _._2 }
  .foreach(i => {
    println(s"costs = ${i._2}")
    i._1.foreach(line => println(line.mkString))
    println("--------------------")
  })

def getNeighbors(
    state: Seq[Seq[String]]
): Map[Seq[Seq[String]], Int] = {
  val players = for {
    y <- 0 until state.size
    x <- 0 until state(2).size
    if Seq("A", "B", "C", "D").contains(state(y)(x))
  } yield (y, x)
  (for {
    player <- players
    moves <- possibleDestinationRoom(
      state,
      player
    ).map(Seq(_)).getOrElse(possibleStepsOnHallway(state, player))
  } yield (
    makeMove(player, moves, state),
    energyMove(player, moves, state)
  )).toMap
}
