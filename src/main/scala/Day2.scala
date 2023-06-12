import zio.http.{Client, ZClient}
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.Source

object Day2 extends ZIOAppDefault {

  sealed trait Hand {
    def value: Int
  }
  private case object Rock extends Hand {
    override def value: Int = 1
  }
  private case object Paper extends Hand {
    override def value: Int = 2
  }
  private case object Scissors extends Hand {
    override def value: Int = 3
  }

  private def compare: (Hand, Hand) => Int = {
    case (Rock, Scissors) => 0
    case (Rock, Rock) => 3
    case (Rock, Paper) => 6
    case (Paper, Rock) => 0
    case (Paper, Paper) => 3
    case (Paper, Scissors) => 6
    case (Scissors, Paper) => 0
    case (Scissors, Scissors) => 3
    case (Scissors, Rock) => 6
  }

  override def run = {
//      val input = ZIO.attempt(Source.fromResource("advent_of_code_day_2_input_test.txt").getLines().toList)
    val input = ZIO.attempt(Source.fromResource("advent_of_code_day_2_input.txt").getLines().toList)

    val index: Map[Char, Hand] = Map(
      'A' -> Rock,
      'B' -> Paper,
      'C' -> Scissors,
      'X' -> Rock,
      'Y' -> Paper,
      'Z' -> Scissors
    )

    input.map { lines =>

      val parse = lines.map(_.split(" ")).map{ case Array(a, b) =>

        (index.get(a.charAt(0)), index.get(b.charAt(0))) match {
          case (Some(a), Some(b)) => compare(a, b) + b.value
          case _ => 0
        }
      }

      println(parse.sum)
    }
  }

}
