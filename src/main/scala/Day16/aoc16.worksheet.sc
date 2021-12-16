val input = io.Source
  .fromFile("src/main/scala/Day16/input.txt")
  .getLines
  .toSeq
  .head

case class Packet(version: Int, packetType: Int, packets: List[Packet])

val packet = "8A004A801A8002F478".flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)

def parseInput(input: String) =
  input.flatMap(c =>
    BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
  )

def parsePacket(packet: String): (BigInt, BigInt) =
  if (packet.size < 11) (999, 999)
  else
    BigInt(packet.slice(3, 6), 2) match {
      case 4 => parseLiteral(packet)
      case 0 => {
        val sub = parseSubPackets(packet.substring(6), _ + _)
        (sub._1, sub._2)
      }
      case 1 => {
        val sub = parseSubPackets(packet.substring(6), _ * _)
        (sub._1, sub._2)
      }
      case 2 => {
        val sub = parseSubPackets(packet.substring(6), _ min _)
        (sub._1, sub._2)
      }
      case 3 => {
        val sub = parseSubPackets(packet.substring(6), _ max _)
        (sub._1, sub._2)
      }
      case 5 => {
        val sub =
          parseSubPackets(packet.substring(6), (l, r) => if (l > r) 1 else 0)
        (sub._1, sub._2)
      }
      case 6 => {
        val sub =
          parseSubPackets(packet.substring(6), (l, r) => if (l < r) 1 else 0)
        (sub._1, sub._2)
      }
      case 7 => {
        val sub =
          parseSubPackets(packet.substring(6), (l, r) => if (l == r) 1 else 0)
        (sub._1, sub._2)
      }
    }

val literal = BigInt("D2FE28", 16).toString(2)

literal.size

def parseLiteral(lit: String) =
  val digits = lit.substring(6).sliding(5, 5).toList
  (
    BigInt(
      digits
        .take(digits.indexWhere(_.startsWith("0")) + 1)
        .map(_.substring(1))
        .mkString,
      2
    ),
    BigInt(
      digits.take(digits.indexWhere(_.startsWith("0")) + 1).mkString.size + 6
    )
  )

parsePacket(literal)

//returns (Int, Int) which are (value, length)
def parseSubPackets(
    packetLoad: String,
    operator: (BigInt, BigInt) => BigInt
): (BigInt, Int) =
  if (packetLoad.head == '0')
    val length = Integer.parseInt(packetLoad.tail.take(15), 2)
    val subpackets = parseNBits(packetLoad.substring(16), length)
    (
      subpackets.map(x => parsePacket(x._1)._1).reduceLeft(operator(_, _)),
      subpackets.map(_._2).max + 22
    )
  else
    val noOfSubpackets = Integer.parseInt(packetLoad.tail.take(11), 2)
    val subpackets = parseNPackets(packetLoad.substring(12), noOfSubpackets)
    (
      subpackets.map(x => parsePacket(x._1)._1).reduceLeft(operator(_, _)),
      subpackets.map(_._2).max + 18
    )

def parseNBits(string: String, length: Int) =
  Iterator
    .iterate((("", string), 0))(x =>
      val p = parsePacket(x._1._2)
      (
        x._1._2.splitAt(p._2.toInt),
        x._2.toInt + p._2.toInt
      )
    )
    .takeWhile(_._2 <= length)
    .toList
    .tail
    .map(x => (x._1._1, x._2))

def parseNPackets(string: String, n: Int) =
  Iterator
    .iterate((("", string), 0))(x =>
      val p = parsePacket(x._1._2)
      (
        x._1._2.splitAt(p._2.toInt),
        x._2.toInt + p._2.toInt
      )
    )
    .take(n + 1)
    .toList
    .tail
    .map(x => (x._1._1, x._2))

val test = "8A004A801A8002F478".flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)

val test2 = "620080001611562C8802118E34".flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)
parsePacket(test2)._1

val test3 = "C0015000016115A2E0802F182340".flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)
parsePacket(test3)._1

val test4 = "A0016C880162017C3686B18A3D4780".flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)
parsePacket(test4)._1

val binaryInput = input.flatMap(c =>
  BigInt(c.toString, 16).toString(2).reverse.padTo(4, '0').reverse
)
binaryInput.length

parsePacket(parseInput("C200B40A82"))
parsePacket(parseInput("04005AC33890"))
parsePacket(parseInput("880086C3E88112"))
parsePacket(parseInput("CE00C43D881120"))
parsePacket(parseInput("D8005AC2A8F0"))
parsePacket(parseInput("F600BC2D8F"))
parsePacket(parseInput("9C005AC2F8F0"))
parsePacket(parseInput("9C0141080250320F1802104A08"))

BigInt("1100001011011000000010010010100001001100", 2)
parsePacket(binaryInput)
