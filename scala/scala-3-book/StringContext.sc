val name = "Scala"
// Scala-CLIだとStringContext("Hello, ", "")とするとコンパイルエラーになる
val sc = scala.StringContext("Hello, ", "")

println(sc.s(name))

extension (sc: scala.StringContext) {
  def printlns(args: Any*): Unit =
    // StringContext.s()を経由することで変数などの埋め込み機能を利用できる
    println(sc.s(args: _*))

  def printnum(args: Int*): Unit = {
    println(sc.s(args: _*))
  }

  def debug(args: Any*): Unit = {
    println(s"parts: ${sc.parts.map(s => s"\"$s\"")}")
    println(sc.parts(0))
    println(s"args: $args")
  }
}

printlns"Hello, $name!"

val x = 1
val y = 2

printnum"$x, $y"

// Int型以外の値を入れようとするとコンパイルエラー
val z = "3"
// printnum"$z"

debug"Hello, $name!"

println(scala.StringContext("Hello, ", "!").s(name))
