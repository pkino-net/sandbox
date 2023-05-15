package com.p_kino.parser_lib_practice

import scala.util.parsing.combinator.JavaTokenParsers

class MtgManaCost extends JavaTokenParsers {
  def generic: Parser[Any] = floatingPointNumber
  def colorless: Parser[Any] = rep("◇")
  def white: Parser[Any] = rep("W")
  def blue: Parser[Any] = rep("U")
  def black: Parser[Any] = rep("B")
  def red: Parser[Any] = rep("R")
  def green: Parser[Any] = rep("G")

  def manaCost: Parser[Any] =
    generic~colorless~white~blue~black~red~green
}

object MtgManaCost extends MtgManaCost {
  def parseManaCost(arg: String) = parse(manaCost, arg)
}
