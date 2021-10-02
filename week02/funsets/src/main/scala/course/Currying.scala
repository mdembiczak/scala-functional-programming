package course

object Currying {

  def main(args: Array[String]): Unit = {
    sumCubes(1, 10) + sumFactorials(10, 20)
    // ==
    sum(x => x * x * x)(1, 10) + sum(fact)(10, 20)

    //Product
    println(product(x => x * x)(1, 5))
    //Factorial
    println(factorial(5))
    //Redefined Sum
    println(redefinedSum(factorial)(1,5))
  }

  // typ (Int => Int) => ((Int, Int) => Int)
  def sum(f: Int => Int): (Int, Int) => Int =
    def sumF(a: Int, b: Int): Int =
      if a < b then 0
      else f(a) + sumF(a + 1, b)

    sumF
  // ==
  def sumShorter(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 0 else f(a) + sumShorter(f)(a + 1, b)


  def sumInts = sum(x => x) // : (Int, Int) => Int

  def sumCubes = sum(x => x * x * x)

  def sumFactorials = sum(fact)

  def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)


  // Excercie
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if a > b then 1 else f(a) * product(f)(a + 1, b)

  def factorial(n: Int) = product(x => x)(1, n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    def recur(a: Int): Int =
      if a > b then zero
      else combine(f(a), recur(a + 1))

    recur(a)

  def redefinedSum(f: Int => Int) = mapReduce(f, (x,y) => x+y, 0)
  def redefinedProduct(f: Int => Int) = mapReduce(f, (x,y) => x*y, 1)
}
