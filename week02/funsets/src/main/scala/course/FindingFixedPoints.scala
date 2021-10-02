package course

import scala.math.abs

object FindingFixedPoints {
  def main(args: Array[String]): Unit = {
    println(sqrt(2))
  }

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
    def iterate(guess: Double): Double =
      val next = f(guess)
      println(next)
      if isCloseEnough(guess, next) then next
      else iterate(next)

    iterate(firstGuess)

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

  def averageDamp(f: Double => Double)(x: Double) =
    (x + f(x)) / 2
}
