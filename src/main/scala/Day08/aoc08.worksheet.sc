val displays = io.Source
  .fromFile("src/main/scala/Day08/input.txt")
  .getLines
  .map(_.split(" \\| ").map(_.split(" ").toList).toList)
  .toList

//Part I
displays.flatMap(_(1)).count(s => uniqeSegmentCombo(s.length))

def uniqeSegmentCombo(noOfSegments: Int): Boolean =
  noOfSegments match {
    case 2 => true // digit 1
    case 4 => true // digit 4
    case 3 => true // digit 7
    case 7 => true // digit 8
    case _ => false
  }

//Part II
def translate(learn: List[String], digits: List[String]): Int =
  digits
    .map(digit => {
      digit.length match {
        case 2 => 1
        case 3 => 7
        case 4 => 4
        case 5 =>
          digit
            .intersect((learn ++ digits).find(_.length == 4).getOrElse(""))
            .length match {
            case 3 =>
              if (
                digit
                  .intersect(
                    (learn ++ digits).find(_.length == 2).getOrElse("")
                  )
                  .length == 2
              ) 3
              else 5
            case _ => 2
          }
        case 6 =>
          if (
            digit
              .intersect((learn ++ digits).find(_.length == 4).getOrElse(""))
              .length == 4
          )
            9
          else if (
            digit
              .intersect((learn ++ digits).find(_.length == 2).getOrElse(""))
              .length == 2
          )
            0
          else
            6
        case 7 => 8
      }
    })
    .mkString
    .toInt

val list = displays.map(d => translate(d(0), d(1)))
list.sum
