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

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight[A, List[B]](as, Nil)((a, acc) => Cons(f(a), acc))

  def join[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight[A, List[B]](as, Nil)((a, acc) => join(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) Cons(a, Nil) else Nil)

  def zipWith[A, B, C](is1: List[A], is2: List[B])(f: (A, B) => C): List[C] = {
    def loop(as: List[A], bs: List[B], acc: List[C]): List[C] =
      (as, bs) match {
        case (Cons(h1, t1), Cons(h2, t2)) =>
          loop(t1, t2, Cons(f(h1, h2), acc))
        case _ =>
          acc
      }

    reverse(loop(is1, is2, Nil))
  }
}

val is1 = List(1, 2, 3)
val is2 = List(4, 5, 6)
println(List.zipWith(is1, is2)(_ + _)) // Cons(5, Cons(7, Cons(9, Nil)))
