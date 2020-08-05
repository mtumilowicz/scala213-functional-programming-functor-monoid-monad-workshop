package answers

import common.Monad

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }
}
