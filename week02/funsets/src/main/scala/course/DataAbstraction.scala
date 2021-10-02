package course

object DataAbstraction {
  def main(args: Array[String]): Unit = {
    val x = OtherRational(1, 3)
    val y = OtherRational(5, 7)
    val z = OtherRational(3, 2)
    println(x.add(y).mul(z))
    println(x.sub(y).sub(z))
    print(x * x + y * y)
  }
}

class OtherRational(x: Int, y: Int):
  require(y > 0, "denominator must be positive")

  //auxiliary constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) then a else gcd(b, a % b)

  val number = x
  val denom = y

  def less(that: OtherRational): Boolean =
    this.number * that.denom < that.number * this.denom

  def max(that: OtherRational): OtherRational =
    if this.less(that) then that else this


  def add(r: OtherRational) =
    OtherRational(number * r.denom + r.number * denom, denom * r.denom)

  def mul(r: OtherRational) =
    OtherRational(number * r.number, denom * r.denom)

  def neg = OtherRational(-number, denom)

  def sub(r: OtherRational) = add(r.neg)

  override def toString: String = s"${number / gcd(x.abs, y)}/${denom / gcd(x.abs, y)}"
  // end marker
end OtherRational


extension (x: OtherRational)
  def +(y: OtherRational): OtherRational = x.add(y)
  def *(y: OtherRational): OtherRational = x.mul(y)
  infix def max(that: OtherRational): OtherRational = x.max(that)
