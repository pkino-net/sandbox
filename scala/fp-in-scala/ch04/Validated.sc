sealed trait Validated[+E, +A] {
  def map[B](f: A => B): Validated[E, B] = this match
    case Valid(value) => Valid(f(value))
    case i @ Invalid(_) => i
  
  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C): Validated[EE, C] =
    (this, b) match
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (i @ Invalid(_), Valid(_)) => i
      case (Valid(_), i @ Invalid(_)) => i
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
  
  /*
  def orElse[EE >: E](b: Validated[EE, A]): Validated[EE, A] =
    (this, b) match
      case (a @ Valid(_), _) => a
      case (_, b @ Valid(_)) => b
      case (Invalid(es1), Invalid(es2)) => Invalid(es1 ++ es2)
  */
}
case class Valid[A](value: A) extends Validated[Nothing, A]
case class Invalid[E](errors: List[E]) extends Validated[E, Nothing]

def traverse[E, A, B](as: List[A])(f: A => Validated[E, B]): Validated[E, List[B]] =
  as.foldRight[Validated[E, List[B]]](Valid(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

def sequence[E, A](as: List[Validated[E, A]]): Validated[E, List[A]] =
  traverse(as)(identity)

type V = Validated[String, Int]

val v1: V = Valid(1)
val v2: V = Valid(2)
val e1: V = Invalid(List("foo"))
val e2: V = Invalid(List("bar"))

println(v1.map2(v2)(_ + _)) // => Valid(3)
println(v1.map2(e1)(_ + _)) // => Invalid(List("foo"))
println(e1.map2(e2)(_ + _)) // => Invalid(List("foo", "bar"))

println(sequence(List(v1, v2)))         // => Valid(List(1, 2))
println(sequence(List(v1, e1, v2, e2))) // => Invalid(List("foo", "bar"))
