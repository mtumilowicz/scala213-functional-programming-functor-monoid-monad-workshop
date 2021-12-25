package structures

import structures.IO2.fix

import scala.annotation.tailrec

trait IO2App {
  def io: IO2[Any]

  def main(args: Array[String]): Unit = {
    io.unsafeRunSync
  }
}

trait IO2[+A] { self =>

  def unsafeRunSync: A = unsafeRunSync(self)

  def flatMap[B](f: A => IO2[B]): IO2[B] = FlatMap(self, f)

  def map[B](f: A => B): IO2[B] = flatMap(f andThen (IO2.succeed(_)))

  def repeat(n: Int): IO2[A] =
    if (n <= 0) self
    else self.flatMap(_ => repeat(n - 1))

  @tailrec
  private def unsafeRunSync[B](io: IO2[B]): B = io match {
    case Succeed(thunk) => thunk()
    case FlatMap(prevIo, f) =>
      val ff = fix(f) // bug in IntelliJ compiler: https://youtrack.jetbrains.com/issue/SCL-13746
      prevIo match {
        case Succeed(thunk) => unsafeRunSync(ff(thunk()))
        case FlatMap(prevPrevIo, g) =>
          val gg = fix(g)
          unsafeRunSync(prevPrevIo flatMap (a => gg(a) flatMap ff))
      }
  }
}

case class Succeed[A](thunk: () => A) extends IO2[A]

case class FlatMap[A, B](io: IO2[A], continuation: A => IO2[B]) extends IO2[B]

object IO2 {

  def succeed[A](a: => A): IO2[A] =
    Succeed(() => a)

  /**
   * https://youtrack.jetbrains.com/issue/SCL-13746
   * IntelliJ compiler bug: Polymorphic recursive method erroneously highlighted as invalid
   * if you compile with other IDE (ex. VS) or with bare scalac you could remove that method
   */
  def fix[A](f: Nothing => IO2[A]): Any => IO2[A] =
    f.asInstanceOf[Any => IO2[A]]
}
