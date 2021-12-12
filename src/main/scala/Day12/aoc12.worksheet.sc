val tunnels = io.Source
  .fromFile("src/main/scala/Day12/input.txt")
  .getLines
  .map(_.split("-").toSeq)
  .toSeq

def findNeighbors(tunnels: Seq[Seq[String]], node: String): Seq[String] =
  tunnels.filter(_.contains(node)).flatten.filter(_ != node)

val upperCase = "[A-Z]+".r

//Part I
def traverse(
    tunnels: Seq[Seq[String]],
    visited: Seq[String],
    node: String
): Int =
  if (node == "end")
    1
  else
    findNeighbors(tunnels, node)
      .diff(visited)
      .map(
        traverse(
          tunnels,
          if (upperCase.matches(node)) visited else visited :+ node,
          _
        )
      )
      .sum

traverse(tunnels, List(), "start")

// Part II
def traverseII(
    tunnels: Seq[Seq[String]],
    visited: Seq[String],
    path: Seq[String],
    node: String,
    smallCave: Boolean
): Set[Seq[String]] =
  if (node == "end")
    Set(path :+ "end")
  else if (node == "start" || upperCase.matches(node) || smallCave)
    findNeighbors(tunnels, node)
      .diff(visited)
      .map(
        traverseII(
          tunnels,
          if (upperCase.matches(node)) visited else visited :+ node,
          path :+ node,
          _,
          smallCave
        )
      )
      .fold(Set())(_ ++ _)
  else
    findNeighbors(tunnels, node)
      .diff(visited)
      .map(traverseII(tunnels, visited, path :+ node, _, true))
      .fold(Set())(_ ++ _) ++
      findNeighbors(tunnels, node)
        .diff(visited)
        .map(traverseII(tunnels, visited :+ node, path :+ node, _, false))
        .fold(Set())(_ ++ _)

traverseII(tunnels, List(), List(), "start", false).size
