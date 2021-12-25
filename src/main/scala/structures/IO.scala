package structures

trait IOApp {
  def run: IO[Any]

  def main(args: Array[String]): Unit = {
    run.unsafeRunSync
  }
}

sealed trait IO[+A] {
  self =>
  def unsafeRunSync: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def unsafeRunSync: B = f(self.unsafeRunSync)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def unsafeRunSync: B = f(self.unsafeRunSync).unsafeRunSync
    }

  def repeat(n: Int): IO[A] =
    if (n <= 0) IO.succeed(unsafeRunSync)
    else self.flatMap(_ => repeat(n - 1))
}

object IO {
  def succeed[A](a: => A): IO[A] = new IO[A] {
    def unsafeRunSync: A = a
  }
}
