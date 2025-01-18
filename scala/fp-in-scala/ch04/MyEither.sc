sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match
    case Right(a) => Right(f(a))
    case l @ Left(_) => l 
    
  def getOrElse[B >: A](default: => B): B = this match
    case Left(_) => default
    case Right(a) => a
    
  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    this match
      case l @ Left(_) => l
      case Right(a) => f(a)
    
  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    this match
      case Left(_) => b
      case r @ Right(_) => r
  
  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      a <- this
      b <- b
    } yield f(a, b)
}
case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]

def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
  if (xs.isEmpty) Left("mean of empty list!")
  else Right(xs.sum / xs.length)

def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
  es.foldRight[MyEither[E, List[A]]](Right(Nil))((e, acc) => e.map2(acc)(_ :: _))

def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
  as.foldRight[MyEither[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))
