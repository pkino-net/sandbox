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

  def reverse[A](as: List[A]): List[A] =
    foldLeft[A, List[A]](as, Nil)((acc, a) => Cons(a, acc))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, a) => f(a, acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight[A, List[A]](as, Nil)((a, acc) => if ((f(a))) Cons(a, acc) else acc)
}

val xs = List(1, 2, 3, 4)
println(List.filter(xs)(_ % 2 == 0)) // Cons(2, Cons(4, Nil))
