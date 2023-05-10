package com.p_kino.parser_lib_practice

object ParseExpr extends Arith {
  def parse(arg: String) = {
    println(s"input: ${arg}")
    println(parseAll(expr, arg))
  }
}
