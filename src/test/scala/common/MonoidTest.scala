package common

import org.scalatest._

class MonoidTest extends propspec.AnyPropSpec
  with org.scalatest.matchers.should.Matchers {

  property("ListConcat") {
    def ListConcat[A]: Monoid[List[A]] = new Monoid[List[A]] {
      def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

      val zero: List[Nothing] = Nil
    }

    def listConcat: Monoid[List[String]] = ListConcat[String]

    listConcat.op(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.op(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.op(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  property("IntSum") {
    def IntSum: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    def intSum: Monoid[Int] = IntSum

    intSum.op(2, 3) shouldBe 5
    intSum.op(intSum.zero, 3) shouldBe 3
    intSum.op(2, intSum.zero) shouldBe 2
    intSum.zero shouldBe 0
  }

  property("IntMultiplication") {
    def IntMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def zero: Int = 1
    }

    def intMultiplication: Monoid[Int] = IntMultiplication

    intMultiplication.op(2, 3) shouldBe 6
    intMultiplication.op(intMultiplication.zero, 3) shouldBe 3
    intMultiplication.op(2, intMultiplication.zero) shouldBe 2
    intMultiplication.zero shouldBe 1
  }

}
