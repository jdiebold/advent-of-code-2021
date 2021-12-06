package Day02

import scala.io.Source
import scala.util.matching.Regex

object Aoc02 {

    def part1(input: String): Int =
        val directions = Source.fromFile(input).getLines.map(_.split(" ").toList).toList
        val pos = directions.foldLeft((0,0))((b,a) => {
            a(0) match {
                case "up" => (b._1 - a(1).toInt, b._2)
                case "down" => (b._1 + a(1).toInt, b._2)
                case "forward" => (b._1, b._2 + a(1).toInt)
            }
        })
        pos._1 * pos._2

    def part2(input: String): Int =
        val directions = Source.fromFile(input).getLines.map(_.split(" ").toList).toList
        val pos = directions.foldLeft((0,0,0))((b,a) => {
            a(0) match {
                case "up" => (b._1, b._2, b._3  - a(1).toInt)
                case "down" => (b._1, b._2,b._3  + a(1).toInt)
                case "forward" => (b._1 + a(1).toInt * b._3, b._2 + a(1).toInt, b._3)
            }
        })
        pos._1 * pos._2
}

@main def main: Unit =
    println(Aoc02.part1("src/main/scala/Day02/input.txt"))
    println(Aoc02.part2("src/main/scala/Day02/input.txt"))