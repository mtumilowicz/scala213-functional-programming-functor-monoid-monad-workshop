package answers

import structures.Applicative

object ApplicativeAnswers {

  def apply[A, B, F[_]](applicative: Applicative[F], fab: F[A => B])(fa: F[A]): F[B] =
    applicative.map2(fab, fa)((f, a) => f(a))

  def map[A, B, F[_]](applicative: Applicative[F], fa: F[A])(f: A => B): F[B] =
    applicative.map2(fa, applicative.unit(()))((a, _) => f(a))

  def sequence[A, F[_]](applicative: Applicative[F], lfa: List[F[A]]): F[List[A]] =
    lfa.foldRight(applicative.unit(List[A]()))((fa, list) => applicative.map2(fa, list)(_ :: _))
}
