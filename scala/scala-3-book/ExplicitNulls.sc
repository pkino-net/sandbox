//> using options "-Yexplicit-nulls"

// NullはStringのサブタイプではなくなったのでコンパイルエラーになる
// val x: String = null

val x: String | Null = null
val y: String | Null = "not null"
val z: String = y.nn

