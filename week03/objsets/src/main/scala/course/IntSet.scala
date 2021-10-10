object Course {
  def main(args: Array[String]): Unit = {

  }
}

abstract class IntSet:
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet

object Empty extends IntSet :
  override def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)

  override def contains(x: Int): Boolean = false

  override def union(other: IntSet): IntSet = other

class NonEmpty(element: Int, left: IntSet, right: IntSet) extends IntSet :
  def incl(x: Int): IntSet =
    if x < element then NonEmpty(element, left.incl(x), right)
    else if x > element then NonEmpty(element, left, right.incl(x))
    else this


  def contains(x: Int): Boolean =
    if x < element then left.contains(x)
    else if x > element then right.contains(x)
    else true

  override def union(other: IntSet): IntSet = left.union(right).union(other).incl(element)

end NonEmpty


object IntSet:
  def apply(): IntSet = Empty
  def apply(x: Int): IntSet = Empty.incl(x)
  def apply(x: Int, y: Int): IntSet = Empty.incl(x).incl(y)
