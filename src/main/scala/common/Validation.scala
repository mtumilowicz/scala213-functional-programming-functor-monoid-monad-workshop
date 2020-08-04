package common

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector.empty[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]