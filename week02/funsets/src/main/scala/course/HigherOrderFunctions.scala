package course

object HigherOrderFunctions {
  def main(args: Array[String]): Unit = {

  }

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if a > b then 0
    else f(a) + sum(f, a + 1, b)


  // without anonymous functions
  def sumInts(a: Int, b: Int): Int = sum(id, a, b)

  def sumCubes(a: Int, b: Int): Int = sum(cube, a, b)

  def sumFactorials(a: Int, b: Int): Int = sum(fact, a, b)

  def id(x: Int): Int = x

  def cube(x: Int): Int = x * x * x

  def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)

  // with anonymous functions
  def anonymousSumInts(a: Int, b: Int) = sum(x => x, a, b)

  def anonymousSumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)

  // excercise
  def tailResursiveSum(f: Int => Int, a: Int, b: Int) =
    def loop(a: Int, acc: Int): Int =
      if a > b then acc
      else loop(a + 1, acc + f(a))

    loop(a, 0)
}
