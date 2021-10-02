package course

object RationalNumbers {
  def main(args: Array[String]): Unit = {
    makeString(addRational(Rational(1, 2), Rational(2, 3)))

    val x = Rational(1, 3)
    val y = Rational(5, 7)
    val z = Rational(3, 2)
    println(x.add(y).mul(z))
    println(x.sub(y).sub(z))
  }
}

def addRational(r: Rational, s: Rational): Rational =
  Rational(r.numer * s.denom + s.numer * r.denom, r.denom * s.denom)

def makeString(r: Rational): String =
  s"${r.numer}/${r.denom}"

class Rational(x: Int, y: Int):
  def numer = x

  def denom = y

  def add(r: Rational) =
    Rational(numer * r.denom + r.numer * denom, denom * r.denom)

  def mul(r: Rational) =
    Rational(numer * r.numer, denom * r.denom)

  def neg = Rational(-numer, denom)

  def sub(r: Rational) = add(r.neg)


  override def toString: String = s"$numer/$denom"

end Rational
