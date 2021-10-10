package course

trait List[T]:
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

class Cons[T](val head: T, val tail: List[T]) extends List[T]:
  override def isEmpty: Boolean = false

class Nil[T] extends List[T]:
  override def isEmpty: Boolean = true

  override def head: T = throw new NoSuchElementException("Nil.head")

  override def tail: List[T] = throw new NoSuchElementException("Nil.tail")


def nth[T](xs: List[T], n: Int): T =
  if(xs.isEmpty) then throw IndexOutOfBoundsException()
  def iter(x: List[T], counter: Int): T =
    if(counter == n) then x.head
    else iter(x.tail, counter + 1)

  iter(xs, 0)


@main def run() =
  println(nth(Cons(1, Cons(2, Cons(3, course.Nil()))), 2))
  println(nth(Cons(1, Cons(2, Cons(3, course.Nil()))), 0))
  println(nth(Cons(1, Cons(2, Cons(3, course.Nil()))), 3))
