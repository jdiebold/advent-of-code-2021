val input = io.Source
  .fromFile("src/main/scala/Day14/input.txt")
  .getLines
  .toSeq

val template = input.head
val rules = input.tail.tail.map(_.split(" -> ").toSeq)

def expand(template: String, rules: Seq[Seq[String]]): String =
  (template
    .sliding(2)
    .toList
    .map(s =>
      rules
        .find(_(0) == s)
        .map(String.valueOf(s(0)) + _(1))
        .getOrElse(s(0))
    ) :+ template.last).mkString

expand(template, rules)
//Part I
val result = Iterator.iterate(template)(expand(_, rules)).take(11).toList.last
val countedResult = result.groupBy(identity).map(i => i._1 -> i._2.length)
countedResult.values.max - countedResult.values.min

//Part II
val last = template.last
val templateCounts =
  template
    .sliding(2)
    .toList
    .groupBy(identity)
    .map(i => i._1 -> i._2.size.toLong)

def expandII(templateCounts: Map[String, Long], rules: Seq[Seq[String]]) =
  templateCounts
    .map(i =>
      rules
        .find(_(0) == i._1)
        .map(c =>
          Seq(
            Map(i._1.head.toString + c.last -> i._2),
            Map(c.last + i._1.last.toString -> i._2)
          )
        )
        .getOrElse(Seq(Map(i)))
    )
    .flatten
    .iterator
    .flatten
    .toList
    .groupMapReduce(_._1)(_._2)(_ + _)

val countedResultII = Iterator
  .iterate(templateCounts)(expandII(_, rules))
  .take(41)
  .toList
  .last
  .groupMapReduce(_._1.head)(_._2)(_ + _)
  .updatedWith(last)(_.map(_ + 1))

countedResultII.values.max - countedResultII.values.min
