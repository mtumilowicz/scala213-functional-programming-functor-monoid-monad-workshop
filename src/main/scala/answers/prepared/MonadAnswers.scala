package answers.prepared

import structures.Monad

object MonadAnswers {
  def maybe: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma match {
        case Some(value) => f(value)
        case None => None
      }
  }
}