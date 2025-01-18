sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size(t: Tree[_]): Int = t match {
    case Leaf(_) =>
      1
    case Branch(t1, t2) =>
      size(t1) + size(t2) + 1
  }

  // 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) =>
      i
    case Branch(t1, t2) =>
      maximum(t1).max(maximum(t2))
  }

  // 3.27
  def depth(t: Tree[_]): Int = t match {
    case Leaf(i) =>
      1
    case Branch(t1, t2) =>
      1 + depth(t1).max(depth(t2))
  }

  // 3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) =>
      Leaf(f(a))
    case Branch(t1, t2) =>
      Branch(map(t1)(f), map(t2)(f))
  }
}

val l = Leaf(1)
println(Tree.map(l)(_ * 2)) // => Leaf(2)
val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
println(Tree.map(t)(_ * 2)) // => Branch(Branch(Leaf(2), Leaf(4)), Leaf(6))
