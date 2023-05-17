package com.p_kino.parser_lib_practice

import scala.util.parsing.combinator.JavaTokenParsers

class MtgManaCostParser extends JavaTokenParsers {
  private def generic: Parser[Int] = opt(wholeNumber) ^^ {
    case None => 0
    case Some(x) => x.toInt
  }
  private def colorless: Parser[Int] = rep("◇") ^^ (_.length)
  private def white: Parser[Int] = rep("W") ^^ (_.length)
  private def blue: Parser[Int] = rep("U") ^^ (_.length)
  private def black: Parser[Int] = rep("B") ^^ (_.length)
  private def red: Parser[Int] = rep("R") ^^ (_.length)
  private def green: Parser[Int] = rep("G") ^^ (_.length)

  def manaCost: Parser[Any] =
    generic~colorless~white~blue~black~red~green ^^ {
      case generic~colorless~w~u~b~r~g =>
        MtgManaCost(generic, colorless, w, u, b, r, g)
    }
}

object MtgManaCostParser extends MtgManaCostParser {
  def parseManaCost(arg: String) = parse(manaCost, arg)
}

case class MtgManaCost (
  generic: Int,
  colorless: Int,
  white: Int,
  blue: Int,
  black: Int,
  red: Int,
  green: Int
)
