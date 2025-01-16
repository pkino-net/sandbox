def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

def add(x: Int, y: Int): Int = x + y

val curried = curry(add)
println(curried(1)(2))

