package answers

object Echo extends App {
  def ReadLine: IO[String] = IO {
    scala.io.StdIn.readLine
  }

  def PrintLine(msg: String): IO[Unit] = IO {
    println(msg)
  }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0


  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def c2 = PrintLine("Enter a temperature in degrees Fahrenheit: ")
    .flatMap(_ => ReadLine.map(_.toDouble).flatMap(d => PrintLine(fahrenheitToCelsius(d).toString)))

  converter.run
  c2.run
}