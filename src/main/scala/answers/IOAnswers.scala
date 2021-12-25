package answers

import structures.{IO, IOApp}

object ConsoleIO {
  def ReadLine: IO[String] =
    IO.succeed(scala.io.StdIn.readLine())

  def PrintLine(msg: String): IO[Unit] =
    IO.succeed(println(msg))
}

object IOAnswers extends IOApp {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  val program: IO[Unit] = for {
    _ <- ConsoleIO.PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ConsoleIO.ReadLine.map(_.toDouble)
    _ <- ConsoleIO.PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  override def run: IO[Any] = program
}
