package answers

import structures.Monad

sealed trait IO[A] {
  self => //  self argument lets us refer to this object as self instead of this
  def run: A // converter definition no longer has side effects—it’s a referentially transparent description of a computation with effects

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}

object IO extends Monad[IO] {
  override def unit[A](a: => A): IO[A] = new IO[A] {
    def run: A = a
  }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}
