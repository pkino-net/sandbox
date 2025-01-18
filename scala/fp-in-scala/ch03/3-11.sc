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

  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    def loop(as: List[A], acc: B): B = as match {
      case Cons(h, t) => loop(t, f(acc, h))
      case Nil => acc
    }

    loop(as, z)
  }

  def sum2(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product2(xs: List[Int]): Int = foldLeft(xs, 1)(_ * _)
}

val xs = List(1, 2, 3)
println(List.sum2(xs))      // => 6
println(List.sum2(Nil))     // => 0
println(List.product2(xs))  // => 6
println(List.product2(Nil)) // => 1
