package Day03

import scala.io.Source

object Aoc03 {
    def part1(input: String): Int =
        val signals = Source.fromFile(input).getLines.toArray.map(_.toCharArray).transpose
        val gamma = signals.map(a => if (a.count(_ == '1') > a.count(_ == '0')) '1' else '0')
        val epsilon = signals.map(a => if (a.count(_ == '1') > a.count(_ == '0')) '0' else '1')
        Integer.parseInt(gamma.mkString(""),2) * Integer.parseInt(epsilon.mkString(""),2)    

    def part2(input: String): Int =
        val signals = Source.fromFile(input).getLines.toArray.map(_.toCharArray)
        val oxygen = filterInputs(signals,0,_ >= _ ).apply(0)
        val co2 = filterInputs(signals,0, _ < _).apply(0)
        Integer.parseInt(oxygen.mkString(""),2) * Integer.parseInt(co2.mkString(""),2)

        
    def filterInputs(signals :Array[Array[Char]], bitpos: Int, comparison: (Int,Int) => Boolean): Array[Array[Char]] =
        if (signals.length == 1)
            signals
        else
            val most = signals.transpose.map(a => if (comparison(a.count(_ == '1'), a.count(_ == '0'))) '1' else '0').apply(bitpos)
            filterInputs(signals.filter(_(bitpos) == most),bitpos + 1,comparison)
}

@main def main: Unit =
    println(Aoc03.part1("src/main/scala/Day03/input.txt"))
    println(Aoc03.part2("src/main/scala/Day03/input.txt"))