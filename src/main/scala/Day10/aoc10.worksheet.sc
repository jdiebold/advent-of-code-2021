import scala.collection.mutable.Stack
val lines = io.Source
  .fromFile("src/main/scala/Day10/input.txt")
  .getLines
  .toList

val pars = Map(
  '(' -> ')',
  '<' -> '>',
  '[' -> ']',
  '{' -> '}'
)

//Part I
def returnFirstCorruptedChar(line: String) =
  val stack = Stack[Char]()
  (0 until line.length)
    .find(i => {
      if (pars.keys.exists(_ == line(i)))
        stack.push(line(i))
        false
      else if (pars.get(stack.top).get == line(i))
        stack.remove(0)
        false
      else true
    })
    .map(line(_))

val scores = Map(
  ')' -> 3,
  ']' -> 57,
  '}' -> 1197,
  '>' -> 25137
)
lines.flatMap(returnFirstCorruptedChar).flatMap(scores.get(_)).sum

//Part II
def autoComplete(line: String) =
  val stack = Stack[Char]()
  val incomplete = (0 until line.length)
    .find(i => {
      if (pars.keys.exists(_ == line(i)))
        stack.push(line(i))
        false
      else if (pars.get(stack.top).get == line(i))
        stack.remove(0)
        false
      else true
    })
    .isEmpty
  if (incomplete) Some(stack.flatMap(pars.get(_)).toList) else None

val scoresII = Map(
  ')' -> 1,
  ']' -> 2,
  '}' -> 3,
  '>' -> 4
)

val lineScores = lines
  .flatMap(autoComplete)
  .map(_.flatMap(scoresII.get(_)).foldLeft(0L)((b, a) => b.*(5) + a))
  .sorted

lineScores(lineScores.length / 2)
