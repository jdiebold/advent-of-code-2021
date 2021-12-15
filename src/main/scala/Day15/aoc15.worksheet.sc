val grid = io.Source
  .fromFile("src/main/scala/Day15/input.txt")
  .getLines
  .toSeq
  .map(_.split("").map(_.toInt).toSeq)

def outOfBounds(grid: Seq[Seq[Int]], y: Int, x: Int): Boolean =
  (x < 0 || y < 0 || y >= grid.length || x >= grid(y).length)

val dirs =
  List((1, 0), (0, 1), (0, -1), (-1, 0))

def getNeighbors(pos: (Int, Int), grid: Seq[Seq[Int]]) =
  dirs
    .flatMap(dir =>
      if (!outOfBounds(grid, pos._1 + dir._1, pos._2 + dir._2))
        Map(
          (
            (pos._1 + dir._1, pos._2 + dir._2),
            grid(pos._1 + dir._1)(pos._2 + dir._2)
          )
        )
      else None
    )
    .toMap

getNeighbors((0, 0), grid)
def dijkstra(graph: Seq[Seq[Int]], source: (Int, Int)) =
  def go(
      active: Set[(Int, Int)],
      res: Map[(Int, Int), Int],
      pred: Map[(Int, Int), (Int, Int)]
  ): (Map[(Int, Int), Int], Map[(Int, Int), (Int, Int)]) =
    if (active.isEmpty) (res, pred)
    else
      val node = active.minBy(res)
      val cost = res(node)
      val neighbors = for {
        (n, c) <- getNeighbors(node, graph)
        if !res.contains(n) && cost + c < res.getOrElse(n, Int.MaxValue)
      } yield n -> (cost + c)
      val active1 = active - node ++ neighbors.keys
      val preds = neighbors.map(i => i._1 -> node)
      go(active1, res ++ neighbors, pred ++ preds)
  end go
  go(Set(source), Map(source -> 0), Map.empty)

(grid.size - 1, grid(0).size - 1)

//Part I
val (distance, predecessors) = dijkstra(grid, (0, 0))
distance(grid.size - 1, grid(0).size - 1)

//Part II
val arrayPlus1 = (g: Seq[Seq[Int]]) =>
  g.map(_.map(i => if (i == 9) 1 else i + 1))

val gridII = Iterator
  .iterate(
    Iterator
      .iterate(grid)(arrayPlus1)
      .take(5)
      .toList
      .transpose
      .map(_.reduceLeft(_ ++ _))
  )(arrayPlus1)
  .take(5)
  .toList
  .flatten

gridII.flatten.size
(gridII.size - 1, gridII(0).size - 1)

val (distanceII, predecessorsII) = dijkstra(gridII, (0, 0))
distanceII(gridII.size - 1, gridII(0).size - 1)
