def compose[A, B, C](f: B => C, g: A => B): A => C =
  a => f(g(a))

val mul2 = (x: Int) => x * 2
val add1 = (x: Int) => x + 1

println(compose(mul2, add1)(3)) // => 8

