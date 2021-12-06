val data = io.Source.fromFile("src/main/scala/Day04/input.txt").getLines.toList
val input = data.head.split(",").map(_.toInt).toList
val players = data.tail
  .sliding(6, 6)
  .map(_.tail.toList)
  .toList
  .map(_.map(_.trim.split("""\s+""").map(_.toInt).toList))

def evaluate(grid: List[List[Int]], numbers: List[Int]): Boolean =
  grid.map(_.intersect(numbers).length == 5).reduce(_ || _) || grid.transpose
    .map(_.intersect(numbers).length == 5)
    .reduce(_ || _)

def evaluateAll(
    players: List[List[List[Int]]],
    numbers: List[Int]
): Option[List[List[Int]]] =
  players.find(evaluate(_, numbers))

def sumOfUnmarkedNumbers(grid: List[List[Int]], numbers: List[Int]): Int =
  grid.map(_.filter(!numbers.contains(_)).sum).sum

//Part I
val lastNumber = input.find(a =>
  evaluateAll(players, input.take(input.indexOf(a) + 1)).isDefined
)
val winner = evaluateAll(players, input.take(input.indexOf(lastNumber.get) + 1))
val sum = sumOfUnmarkedNumbers(
  winner.get,
  input.take(input.indexOf(lastNumber.get) + 1)
)
sum * lastNumber.get

//Part II
def findLastMatch(
    players: List[List[List[Int]]],
    numbers: List[Int]
): List[List[List[Int]]] =
  if (players.length == 1) players
  else {
    val lastNumber = numbers.find(a =>
      evaluateAll(players, numbers.take(numbers.indexOf(a) + 1)).isDefined
    )
    findLastMatch(
      players.filter(a =>
        !evaluate(a, numbers.take(numbers.indexOf(lastNumber.get) + 1))
      ),
      numbers
    )
  }

val rest =
  players.filter(!evaluate(_, input.take(input.indexOf(lastNumber.get) + 1)))

val lastWinner = findLastMatch(players, input)
val lastWinnerNo =
  input.find(a => evaluate(lastWinner(0), input.take(input.indexOf(a) + 1)))
sumOfUnmarkedNumbers(
  lastWinner(0),
  input.take(input.indexOf(lastWinnerNo.get) + 1)
) * lastWinnerNo.get
