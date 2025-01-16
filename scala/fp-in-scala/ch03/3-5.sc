sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case l @ Cons(h, t) =>
      if (f(h)) dropWhile(t, f) else l
    case l => l
  }
}

val xs = List(1, 2, 3)
println(List.dropWhile(xs, _ <= 2))               // => Cons(3, Nil)
println(List.dropWhile(xs, _ <= 3))               // => Nil
println(List.dropWhile((Nil: List[Int]), _ <= 2)) // => Nil

