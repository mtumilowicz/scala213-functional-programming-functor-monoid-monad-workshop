package answers

import structures.Functor

object FunctorAnswers {
  def listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

  def optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
  }

  def distribute[A, B, F[_]](f: Functor[F], fab: F[(A, B)]): (F[A], F[B]) =
    (f.map(fab)(_._1), f.map(fab)(_._2))
}
