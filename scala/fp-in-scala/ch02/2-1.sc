def fib(n: Int): Int = {
  def loop(x: Int, y: Int, n: Int): Int =
    if (n == 1)
      x
    else loop(y, x + y, n - 1)

  loop(0, 1, n)
}

(1 to 6).foreach(x => println(fib(x)))

