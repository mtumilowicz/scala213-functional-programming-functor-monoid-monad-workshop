package common

import answers.IO

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def sequence[A](lfa: List[F[A]]): F[List[A]] =
  traverse(lfa)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))
}