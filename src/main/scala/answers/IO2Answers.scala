package answers

import structures.{IO2, IO2App}

object Console2 {
  def ReadLine: IO2[String] = IO2 succeed {
    scala.io.StdIn.readLine()
  }

  def PrintLine(msg: String): IO2[Unit] = IO2 succeed {
    println(msg)
  }
}

object EchoAnswers2 extends IO2App {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  val program: IO2[Unit] = for {
    _ <- Console2.PrintLine("Enter a temperature in degrees Fahrenheit: ") // verify that the stackoverflow does not occur
    d <- Console2.ReadLine.map(_.toDouble)
    _ <- Console2.PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  override def io: IO2[Any] = program
}
