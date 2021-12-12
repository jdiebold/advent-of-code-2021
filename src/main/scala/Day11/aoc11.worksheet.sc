val grid = io.Source
  .fromFile("src/main/scala/Day11/test.txt")
  .getLines
  .map(_.split("").map(_.toInt).toSeq)
  .toSeq

val dirs =
  List((-1, -1), (-1, 0), (0, -1), (-1, 1), (1, -1), (1, 0), (0, 1), (1, 1))

def neighborFlashed(grid: Seq[Seq[Int]], y: Int, x: Int): Int =
  dirs.collect {
    case pos
        if !outOfBounds(grid, y + pos._1, x + pos._2) && grid(y + pos._1)(
          x + pos._2
        ) >= 10 =>
      pos
  }.size

def outOfBounds(grid: Seq[Seq[Int]], y: Int, x: Int): Boolean =
  (x < 0 || y < 0 || y >= grid.length || x >= grid(y).length)

def makeStep(grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
  val updatedGrid = grid.map(_.map(_ + 1))
  def flash(grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
    if (!grid.flatten.exists(_ >= 10)) grid
    else
      flash(increaseNeighbors(grid))
  flash(updatedGrid)

def increaseNeighbors(grid: Seq[Seq[Int]]): Seq[Seq[Int]] =
  (0 until grid.length).map(y => {
    (0 until grid(y).size).map(x => {
      if (grid(y)(x) < 10 && grid(y)(x) != 0)
        grid(y)(x) + neighborFlashed(grid, y, x)
      else if (grid(y)(x) >= 10) 0
      else grid(y)(x)
    })
  })

//Part I
(1 to 100)
  .foldLeft((grid, 0))((b, a) => {
    val newGrid = makeStep(b._1)
    (newGrid, b._2 + newGrid.flatten.count(_ == 0))
  })
  ._2

//Part II
Iterator
  .iterate(grid)(makeStep)
  .takeWhile(g => !(g.map(_.max).max == 0))
  .size
