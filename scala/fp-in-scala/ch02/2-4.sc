def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a, b) => f(a)(b)

val add = (x: Int) => (y: Int) => x + y
println(uncurry(add)(1, 2))

