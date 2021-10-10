package course

abstract class Nat:
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
end Nat

object Zero extends Nat:
  override def isZero: Boolean = true

  override def predecessor: Nat = ???

  override def successor: Nat = Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if that.isZero then this else ???

  override def toString: String = "Zero"
end Zero


class Succ(n: Nat) extends Nat:
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = Succ(this)

  override def +(that: Nat): Nat = Succ(n + that)

  override def -(that: Nat): Nat = if that.isZero then this else n - that.predecessor

  override def toString: String = s"Succ($n)"
end Succ


@main def runNat() =
  val two = Succ(Succ(Zero))
  val one = Succ(Zero)
  println(two + one)
