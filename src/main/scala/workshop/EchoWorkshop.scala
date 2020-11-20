package workshop

import structures.IO

object EchoWorkshop extends App {

  def ReadLine: IO[String] = null // hint: IO, scala.io.StdIn.readLine()

  def PrintLine(msg: String): IO[Unit] = null // hint: IO, println

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = null // hint: for comprehension
  // print line: Enter a temperature in degrees Fahrenheit
  // read line, hint: convert to double
  // convert to celsius and print, hint: PrintLine

  // run converter, hint: converter.run
}
