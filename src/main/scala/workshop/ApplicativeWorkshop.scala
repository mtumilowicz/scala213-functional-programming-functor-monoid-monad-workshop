package workshop

import structures.Applicative

object ApplicativeWorkshop {

  def apply[A, B, F[_]](applicative: Applicative[F], fab: F[A => B])(fa: F[A]): F[B] = ???
  // use map2

  def map[A, B, F[_]](applicative: Applicative[F], fa: F[A])(f: A => B): F[B] = ???
  // map2 + unit

  def sequence[A, F[_]](applicative: Applicative[F], lfa: List[F[A]]): F[List[A]] = ???
  //foldRight, unit, map2, concat
}
