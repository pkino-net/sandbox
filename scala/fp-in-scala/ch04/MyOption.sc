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
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]
