
package FizzBuzz

object Strategy {
  type Strategy = Int => String
}

import Strategy.Strategy

case class Game(defaultStrategy: Strategy, strategies: List[Strategy]) {
  def play(input: Int): String = {
    val result: String = strategies.foldLeft("")(_ + _(input))
    result match {
      case "" => defaultStrategy(input)
      case _ => result
    }
  }
}

case class AppRunner(game: Game, destination: Int) {
  var runningNumber = 1

  def run(): String = {
    val input: String = io.StdIn.readLine()
    if (game.play(runningNumber) == input && runningNumber < destination) {
      runningNumber += 1
      run()
    }
    if (runningNumber >= destination) "Won" else "Lost"
  }
}


object FizzBuzz {
  def divisionStrategy(divisor: Int, name: String)(input: Int) =
    if (input % divisor == 0) name else ""

  def defaultStrategy(input: Int) = input.toString

  def main(args: Array[String]) {
    val strategies: List[(Int) => String] = List(divisionStrategy(2,"Fizz"), divisionStrategy(3,"Buzz"))
    val game: Game = Game(defaultStrategy, strategies)
    val appRunner: AppRunner = AppRunner(game, 10)
    println(appRunner.run())
  }
}