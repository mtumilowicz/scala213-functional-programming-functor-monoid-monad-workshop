package common

trait Monad[F[_]] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

//  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
//    a => flatMap(f(a))(g)

//  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
//    ms match {
//      case Nil => unit(Nil)
//      case h :: t => flatMap(f(h))(b =>
//        if (!b) filterM(t)(f)
//        else map(filterM(t)(f))(h :: _))
//    }

//  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
//    flatMap(fa)(a => map(fb)(b => f(a, b)))

}