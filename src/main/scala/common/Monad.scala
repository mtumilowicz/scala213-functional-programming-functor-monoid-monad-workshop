package common

import answers.IO

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  //  def sequence[A](lma: List[F[A]]): F[List[A]]
  //  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]]
}