package common

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  //  def sequence[A](lma: List[F[A]]): F[List[A]]
  //  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]]
}

sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = f(self.run).run
    }
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa map f

  def apply[A](a: => A): IO[A] = unit(a)
}

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

  converter.run
}