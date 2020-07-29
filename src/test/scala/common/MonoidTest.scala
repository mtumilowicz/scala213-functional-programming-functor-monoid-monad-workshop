package common

class MonoidTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("ListConcat") {
    def listConcat[A]: Monoid[List[A]] = new Monoid[List[A]] {
      override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

      override val zero: List[Nothing] = Nil
    }

    listConcat.op(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.op(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.op(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  test("IntSum") {
    def intSum: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    intSum.op(2, 3) shouldBe 5
    intSum.op(intSum.zero, 3) shouldBe 3
    intSum.op(2, intSum.zero) shouldBe 2
    intSum.zero shouldBe 0
  }

  test("IntMultiplication") {
    def intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def zero: Int = 1
    }

    intMultiplication.op(2, 3) shouldBe 6
    intMultiplication.op(intMultiplication.zero, 3) shouldBe 3
    intMultiplication.op(2, intMultiplication.zero) shouldBe 2
    intMultiplication.zero shouldBe 1
  }

}
