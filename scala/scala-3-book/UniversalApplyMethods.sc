class C

// constructor proxy companion objectは値として扱えないので以下はエラー
// println(C)

class Foo

// applyというメンバが無いためconstructor proxy applyが生える
object Foo

println(Foo())

class Bar

object Bar {
  private val apply = "apply"
}

// companion objectに既にapplyという名前のメンバがあるのでconstructor proxy applyは生えてこない
// println(Bar())

