package workshop

import structures.{IO, IOApp}

object ConsoleIO {
  def ReadLine: IO[String] = null // hint: IO succeed, scala.io.StdIn.readLine()

  def PrintLine(msg: String): IO[Unit] = null // hint: IO succeed, println
}

object EchoWorkshop extends IOApp {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  val program: IO[Unit] = null // hint: for comprehension
  // print line: Enter a temperature in degrees Fahrenheit; observe how it overflow stack if repeated 1_000_000 times
  // read line, hint: convert to double
  // convert to celsius and print, hint: PrintLine

  override def run: IO[Any] = program
}
