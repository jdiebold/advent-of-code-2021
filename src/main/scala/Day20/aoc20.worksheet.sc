val input = io.Source
  .fromFile("src/main/scala/Day20/input.txt")
  .getLines

val enhancer = input.next
val image = input.toSeq.tail.map(_.split("").toSeq)

def enhancePixel(
    image: Seq[Seq[String]],
    y: Int,
    x: Int,
    enhancer: String,
    filler: String
): String =
  enhancer(
    Integer.parseInt(
      (y - 1 to y + 1)
        .map(iy =>
          (x - 1 to x + 1)
            .map(ix =>
              image.lift(iy).getOrElse(Seq(filler)).lift(ix).getOrElse(filler)
            )
            .mkString
        )
        .mkString
        .replace(".", "0")
        .replace("#", "1"),
      2
    )
  ).toString

def enhanceImage(
    image: Seq[Seq[String]],
    enhancer: String,
    fillString: String
): (Seq[Seq[String]], String) =
  (
    (-1 to image.size)
      .map(y =>
        (-1 to image(0).size)
          .map(x => enhancePixel(image, y, x, enhancer, fillString))
      ),
    enhancePixel(
      List.fill(3)(List.fill(3)(fillString)),
      1,
      1,
      enhancer,
      fillString
    )
  )

Iterator
  .iterate((image, "."))(i => enhanceImage(i._1, enhancer, i._2))
  .take(51)
  .toSeq
  .last
  ._1
  .map(_.count(_ == "#"))
  .sum
