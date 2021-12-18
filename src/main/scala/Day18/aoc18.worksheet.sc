import pprint.Tree.Lazy
import scala.util.matching.Regex.Match
val input = io.Source
  .fromFile("src/main/scala/Day18/input.txt")
  .getLines
  .toSeq

def explode(string: String): String =
  val p = """\[\d+,\d+\]""".r
    .findAllMatchIn(string)
    .collectFirst {
      case x if nested4Times(x) => x
    }
    .map(m => (m.matched, m.before.toString, m.after.toString))
    .get
  val pN = p._1
    .split(",")
    .toSeq
    .map(_.filter(_.isDigit).mkString)
    .map(_.toInt)
  val a = """\d+""".r
    .findAllMatchIn(p._2)
    .toList
    .lastOption
    .map(m => m.before.toString + (m.matched.toInt + pN(0)) + m.after.toString)
    .getOrElse(p._2)
  val b = """\d+""".r
    .findFirstMatchIn(p._3)
    .map(m =>
      m.source.toString
        .replaceFirst(m.matched, (m.matched.toInt + pN(1)).toString)
    )
    .getOrElse(p._3)
  a + "0" + b

explode("[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]")
val i = "[[[[12,12],[6,14]],[[15,0],[17,[8,1]]]],[2,9]]"
val p = """\[\d+,\d+\]""".r
  .findAllMatchIn(i)
  .collectFirst {
    case x if nested4Times(x) => x
  }
  .map(m => (m.matched, m.before.toString, m.after.toString))
  .get

val pN = p._1
  .split(",")
  .toSeq
  .map(_.filter(_.isDigit).mkString)
  .map(_.toInt)
pN(0)
"""\d+""".r
  .findAllMatchIn(p._2)
  .toList
  .lastOption
  .map(m => m.before.toString + (m.matched.toInt + pN(0)) + m.after.toString)
"""\d+""".r
  .findAllMatchIn(p._2)
  .toList
  .lastOption
  .map(m => m.before.toString + (m.matched.toInt + pN(0)) + m.after.toString)
  .getOrElse(p._2)

def nested4Times(pair: Match): Boolean =
  pair.before.toString.count(_ == '[') - pair.before.toString.count(
    _ == ']'
  ) >= 4

def split(string: String): String =
  """\d{2}""".r
    .findFirstMatchIn(string)
    .map(m =>
      m.source.toString
        .replaceFirst(
          m.matched,
          "[" + (m.matched.toInt / 2).toString + "," + Math
            .ceil(m.matched.toDouble / 2)
            .toInt
            .toString + "]"
        )
    )
    .get

def mustExplode(string: String): Boolean =
  """\[\d+,\d+\]""".r
    .findAllMatchIn(string)
    .collectFirst {
      case x if nested4Times(x) => x
    }
    .isDefined

def mustSplit(string: String): Boolean =
  """\d{2}""".r
    .findFirstMatchIn(string)
    .isDefined

def explodeOrSplit(string: String): String =
  if (mustExplode(string))
    explode(string)
  else if (mustSplit(string))
    split(string)
  else
    string

def reduce(l: String, r: String): String =
  Iterator
    .iterate(s"[$l,$r]")(explodeOrSplit)
    .to(LazyList)
    .find(s => !(mustExplode(s) || mustSplit(s)))
    .get

def add(l: String, r: String): String =
  s"[$l,$r]"

def convert(string: Option[String]): Option[String] =
  """\[\d+,\d+\]""".r
    .findFirstMatchIn(string.getOrElse(""))
    .map(m =>
      m.before.toString + m.matched
        .split(",")
        .map(_.filter(_.isDigit).mkString.toInt)
        .reduceLeft(3 * _ + 2 * _) + m.after.toString
    )

def calcMagnitude(string: String): Option[String] =
  Iterator
    .iterate(Some(string))(convert)
    .takeWhile(_.isDefined)
    .toSeq
    .last

// Part I
calcMagnitude(input.reduceLeft(reduce))

// Part II
input
  .combinations(2)
  .toSeq
  .flatMap(l => List(l, l.reverse))
  .map(x => reduce(x(0), x(1)))
  .flatMap(calcMagnitude)
  .map(_.toInt)
  .max
