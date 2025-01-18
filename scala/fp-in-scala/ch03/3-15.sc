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

  def join[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons)
}

val xs1 = List(1, 2, 3)
val xs2 = List(4, 5, 6)
println(List.join(xs1, xs2)) // Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
