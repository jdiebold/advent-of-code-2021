val crabs = io.Source
  .fromFile("src/main/scala/Day07/input.txt")
  .getLines
  .next
  .split(",")
  .map(_.toInt)
  .toSeq

//Part I
(crabs.min to crabs.max).map(f => crabs.map(n => (n - f).abs).sum).min

// Part II
def fuelConsumption(start: Int, goal: Int): Int =
  val diff = (start - goal).abs
  diff * (diff + 1) / 2

(crabs.min to crabs.max).map(f => crabs.map(fuelConsumption(f, _)).sum).min
