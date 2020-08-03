package answers

import common.{IO, Monad}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  override def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa map f

  def apply[A](a: => A): IO[A] = unit(a)
}
