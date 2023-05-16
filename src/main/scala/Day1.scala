import zio.Console
import zio.http.{Client, ZClient}
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.Source

object Day1 extends ZIOAppDefault {

  override def run = {
    //    val input = ZIO.attempt(Source.fromResource("advent_of_code_day_1_input_test.txt").getLines().toList)
    val input = ZIO.attempt(Source.fromResource("advent_of_code_day_1_input.txt").getLines().toList)

    final case class State(acc: Int = 0, listAcc: List[Int] = List.empty)

    input.map(inputList =>
      val accumulator = inputList.foldLeft(State()) { (stateAcc, stringInput) =>
        if (stringInput.isEmpty) {
          stateAcc.copy(acc = 0, stateAcc.acc :: stateAcc.listAcc)
        } else {
          val number = stringInput.toInt
          stateAcc.copy(acc = stateAcc.acc + number)
        }
      }
      accumulator.listAcc :+ accumulator.acc
    )
      .tap(result =>
        Console.printLine(result.max.toString)
      )
      .tap(result =>
        Console.printLine(result.sortWith(_ > _).take(3).sum)
      )
  }

}
