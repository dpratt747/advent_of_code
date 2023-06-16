import zio.http.{Client, ZClient}
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.Source

object Day3 extends ZIOAppDefault {

  private val priorityMap = Map(
    'a' -> 1,
    'b' -> 2,
    'c' -> 3,
    'd' -> 4,
    'e' -> 5,
    'f' -> 6,
    'g' -> 7,
    'h' -> 8,
    'i' -> 9,
    'j' -> 10,
    'k' -> 11,
    'l' -> 12,
    'm' -> 13,
    'n' -> 14,
    'o' -> 15,
    'p' -> 16,
    'q' -> 17,
    'r' -> 18,
    's' -> 19,
    't' -> 20,
    'u' -> 21,
    'v' -> 22,
    'w' -> 23,
    'x' -> 24,
    'y' -> 25,
    'z' -> 26,
    'A' -> 27,
    'B' -> 28,
    'C' -> 29,
    'D' -> 30,
    'E' -> 31,
    'F' -> 32,
    'G' -> 33,
    'H' -> 34,
    'I' -> 35,
    'J' -> 36,
    'K' -> 37,
    'L' -> 38,
    'M' -> 39,
    'N' -> 40,
    'O' -> 41,
    'P' -> 42,
    'Q' -> 43,
    'R' -> 44,
    'S' -> 45,
    'T' -> 46,
    'U' -> 47,
    'V' -> 48,
    'W' -> 49,
    'X' -> 50,
    'Y' -> 51,
    'Z' -> 52
  )

  override def run = {
//      val input = ZIO.attempt(Source.fromResource("advent_of_code_day_3_input_test.txt").getLines().toList)
    val input = ZIO.attempt(Source.fromResource("advent_of_code_day_3_input.txt").getLines().toList)

      input.map{ lines =>
        lines.flatMap { string =>
          val middle = string.length / 2
          val compartmentA = string.substring(0, middle)
          val compartmentB = string.substring(middle)

          compartmentA.distinct.filter(char => compartmentB.contains(char)).toList.map(char => priorityMap(char))
        }
      }.tap(a => Console.printLine(a.sum))

  }

}
