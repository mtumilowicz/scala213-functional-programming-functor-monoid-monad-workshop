package workshop

import structures.Functor

object FunctorWorkshop {
  def listFunctor: Functor[List] = null // Functor[List], hint: use list map

  def optionFunctor: Functor[Option] = null // Functor[Option], hint: use option map

  def distribute[A, B, F[_]](f: Functor[F], fab: F[(A, B)]): (F[A], F[B]) = null
  // hint: use tuple
}
