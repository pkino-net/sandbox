import scala.util.CommandLineParser

enum Color:
  case Red, Green, Blue

given CommandLineParser.FromString[Color] with
  def fromString(value: String): Color = Color.valueOf(value)

@main def run(color: Color): Unit =
  println(s"The color is ${color.toString}")
