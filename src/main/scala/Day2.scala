import zio.http.{Client, ZClient}
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.Source

object Day2 extends ZIOAppDefault {

  private sealed trait Outcome
  private case object Win extends Outcome
  private case object Draw extends Outcome
  private case object Lose extends Outcome

  private sealed trait Hand {
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

  private def getTargetHand: (Hand, Outcome) => Hand = {
    case (Rock, Lose) => Scissors
    case (Rock, Draw) => Rock
    case (Rock, Win) => Paper
    case (Paper, Lose) => Rock
    case (Paper, Draw) => Paper
    case (Paper, Win) => Scissors
    case (Scissors, Lose) => Paper
    case (Scissors, Win) => Rock
    case (Scissors, Draw) => Scissors
  }

  private def compareHands: (Hand, Hand) => Int = {
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

    val firstColumnLookup: Map[Char, Hand] = Map(
      'A' -> Rock,
      'B' -> Paper,
      'C' -> Scissors
    )

    val secondColumnLookup: Map[Char, Outcome] = Map(
      'X' -> Lose,
      'Y' -> Draw,
      'Z' -> Win
    )

    input.map { lines =>

      lines.map(_.split(" ")).map{ case Array(a, b) =>

        (firstColumnLookup.get(a.charAt(0)), secondColumnLookup.get(b.charAt(0))) match {
          case (Some(playerOnesHand), Some(outcome)) =>
            val targetHand = getTargetHand(playerOnesHand, outcome)
            compareHands(playerOnesHand, targetHand) + targetHand.value
          case _ => 0 // should not happen
        }
      }

    }.tap(list => Console.printLine(list.sum))
  }

}
