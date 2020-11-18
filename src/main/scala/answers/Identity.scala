package answers

import common.Monad

case class Identity[A](value: A) {
  def map[B](f: A => B): Identity[B] = Identity(f(value))

  def flatMap[B](f: A => Identity[B]): Identity[B] = f(value)
}

object Identity {
  val idMonad: Monad[Identity] = new Monad[Identity] {
    def unit[A](a: => A): Identity[A] = Identity(a)

    def flatMap[A, B](ida: Identity[A])(f: A => Identity[B]): Identity[B] = ida flatMap f
  }
}
