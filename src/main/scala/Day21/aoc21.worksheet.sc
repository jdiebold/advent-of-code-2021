val input = io.Source
  .fromFile("src/main/scala/Day21/input.txt")
  .getLines
  .toSeq
  .map("\\d+".r.findAllMatchIn(_).toSeq.last.matched.toInt)

val deterministicDie = Iterator.iterate(1)(_ + 1).zipWithIndex

//player = (score, position)
def takeTurn(
    players: Seq[(Int, Int)],
    die: Iterator[(Int, Int)],
    nextPlayer: Int
): (Seq[(Int, Int)], Int) =
  val player = players(nextPlayer)
  val step = die.take(3).map(_._1).sum
  val newPos =
    if ((player._2 + step) % 10 != 0) ((player._2 + step) % 10) else 10
  (
    players.updated(nextPlayer, (player._1 + newPos, newPos)),
    (nextPlayer + 1) % players.length
  )

val players = Seq.fill(2)(0).zip(input)

//Part I
val finalScore = Iterator
  .iterate((players, 0))(i => takeTurn(i._1, deterministicDie, i._2))
  .find(_._1.map(_._1).max >= 1000)
  .get

deterministicDie.next._2 * finalScore._1.map(_._1).min

// Part II
// 1x steps 3 , 9
1 + 1 + 1
3 + 3 + 3

// 3x steps 4,8
1 + 1 + 2
3 + 3 + 2
// 6x  step 5
1 + 1 + 3
2 + 2 + 1

//1*6+1*1=  7x step 6
1 + 2 + 3 // *6
2 + 2 + 2

// 2* 3 = 6 x step 7
2 + 2 + 3
3 + 3 + 1

// steps -> how many times
val possibleOutcomes =
  Map(3 -> 1, 9 -> 1, 4 -> 3, 5 -> 6, 8 -> 3, 6 -> 7, 7 -> 6)

def calcQuantumWinner(
    players: Seq[(Int, Int)],
    nextPlayer: Int
): Seq[Long] =
  if (players.map(_._1).max >= 21)
    Seq.fill(players.length)(0L).updated(players.indexWhere(_._1 >= 21), 1L)
  else
    val player = players(nextPlayer)
    possibleOutcomes
      .map(o =>
        val newPos =
          if ((player._2 + o._1) % 10 != 0) ((player._2 + o._1) % 10) else 10
        calcQuantumWinner(
          players.updated(nextPlayer, (player._1 + newPos, newPos)),
          (nextPlayer + 1) % players.length
        ).map(_ * o._2)
      )
      .reduceLeft((l, r) => l.zip(r).map(i => i._1 + i._2))

calcQuantumWinner(players, 0)
//
//0 -> 570239341223618
//
//1 -> 371697814511699
