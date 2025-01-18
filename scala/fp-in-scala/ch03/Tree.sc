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
}

val l = Leaf(1)
println(Tree.size(l)) // => 1
val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
println(Tree.size(t)) // => 5
