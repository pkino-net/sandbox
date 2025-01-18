/**
  * Optionという型名にするとScalaにコンパイルした際にSTLのOptionを使ったコードが
  * 追加された際に型名の衝突を起こし、ここで実装したOptionに存在しないメソッドを
  * 呼び出そうとしてコンパイルエラーになるため、MyOptionという型名にする
  */
sealed trait MyOption[+A] {
  // 4.1

  def map[B](f: A => B): MyOption[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  /**
   * map(f).getOrElse(None) で実装できる
   */
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f) match {
    case Some(Some(b)) => Some(b)
    case _ => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  /**
   * map(Some).getOrElse(ob) で実装できる
   */
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    if (this != None) this else ob
  
  // 4.2
  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

def mean(xs: Seq[Double]): MyOption[Double] =
  if (xs.isEmpty) None else Some(xs.sum / xs.length)

// 4.3
def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  a.flatMap(a => b.map(b => f(a, b)))

def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] =
  as.foldRight[MyOption[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
  as.foldRight[MyOption[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

val l1 = List(List(1.0, 2.0), List(3.0, 4.0))
val l2 = List(List(1.0, 2.0), Nil)
println(traverse(l1)(mean)) // => Some(List(1.5, 3.5))
println(traverse(l2)(mean)) // => None
