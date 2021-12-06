package Day01

import scala.io.Source
import scala.compiletime.ops.string

object Aoc01 {
    def aoc01_a(input: String): Int = {
        val lines = Source.fromFile(input).getLines.map(_.toInt).toList   
        lines.sliding(2).filter(a => a(0) < a(1)).size
    }

    def aoc01_b(input: String): Int = {
        val lines = Source.fromFile(input).getLines.map(_.toInt).toList   
        lines.sliding(3).map(_.sum).sliding(2).count(a => a(0) < a(1))
    }
}

@main def main: Unit =
    println(Aoc01.aoc01_b("src/main/scala/Day01/input.txt"))    