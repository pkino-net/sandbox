package com.p_kino.parser_lib_practice

import scala.util.parsing.combinator.JavaTokenParsers

class JSON extends JavaTokenParsers {
  def value: Parser[Any] = obj | arr | stringLiteral | floatingPointNumber | "null" | "true" | "false"

  def obj: Parser[Any] = "{"~repsep(member, ",")~"}"

  def arr: Parser[Any] = "["~repsep(value, ",")~"]"

  def member: Parser[Any] = stringLiteral~":"~value
}

object JSON extends JSON {
  def parseJson(json: String): ParseResult[Any] = parseAll(value, json)

  val addressBook =
    s"""
       |{
       |  "address book": {
       |    "name": "John Smith",
       |    "address": {
       |      "street": "10 Market Street",
       |      "city": "San Francisco, CA",
       |      "zip": 94111
       |    },
       |    "phone numbers": [
       |      "408 338-4238",
       |      "408 111-6892"
       |    ]
       |  }
       |}
       |""".stripMargin
}
