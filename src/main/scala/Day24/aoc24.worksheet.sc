import scala.util.control.Breaks._
import scala.collection.mutable.Stack
val cmds = io.Source
  .fromFile("src/main/scala/Day24/input.txt")
  .getLines
  .toSeq
  .map(_.split(" ").toSeq)
  .sliding(18, 18)
  .toSeq
  .map(block => (block(4)(2).toInt, block(5)(2).toInt, block(15)(2).toInt))
  .iterator

val start =
  Map("w" -> 0L, "x" -> 0L, "y" -> 0L, "z" -> 0L)

def calcCmd(
    cmd: Seq[String],
    state: Map[String, Long],
    input: Iterator[Int]
): Map[String, Long] =
  state.updated(
    cmd(1),
    cmd(0) match {
      case "inp" => input.next
      case "add" => state(cmd(1)) + state.getOrElse(cmd(2), cmd(2).toLong)
      case "mul" => state(cmd(1)) * state.getOrElse(cmd(2), cmd(2).toLong)
      case "div" => state(cmd(1)) / state.getOrElse(cmd(2), cmd(2).toLong)
      case "mod" => state(cmd(1)) % state.getOrElse(cmd(2), cmd(2).toLong)
      case "eql" =>
        if (state(cmd(1)) == state.getOrElse(cmd(2), cmd(2).toLong)) 1
        else 0
    }
  )

val res = Iterator
  .iterate(Seq((0L, "")))(it =>
    val cmd = cmds.next
    //println(s"c1 = ${cmd(4)(2)}, c2= ${cmd(5)(2)}, c3=${cmd(15)(2)} ")
    deduplicateState(
      it.flatMap((z, path) =>
        (1 to 9).map(in =>
          (
            calcBlock(cmd, z, in),
            path + in.toString
          )
        )
      ),
      false
    )
  )
  .drop(14)
  .next
  .sortBy(_._1)

def deduplicateState(
    states: Seq[(Long, String)],
    max: Boolean
): Seq[(Long, String)] =
  states
    .filter(_._1 <= Math.pow(10, 7))
    .groupBy(_._1)
    .map(i => if (max) i._2.maxBy(_._2) else i._2.minBy(_._2))
    .toSeq

def calcBlock(cmd: (Int, Int, Int), state: Long, w: Int) =
  if (state % 26 + cmd._2 == w)
    state / cmd._1
  else
    state / cmd._1 * 26 + cmd._3 + w
