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

  test("optionMonoid") {
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

      override def zero: Option[A] = Option.empty
    }

    optionMonoid.op(Some(1), Some(2)) shouldBe Some(1)
    optionMonoid.op(Some(1), Option.empty) shouldBe Some(1)
    optionMonoid.op(Option.empty, Some(1)) shouldBe Some(1)
    optionMonoid.zero shouldBe Option.empty
  }

  test("endoMonoid") {
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

      override def zero: A => A = a => a
    }

    endoMonoid.op((a: Int) => a * 2, (a: Int) => a * 3)(1) shouldBe 6
    endoMonoid.op(endoMonoid.zero, (a: Int) => a * 3)(1) shouldBe 3
    endoMonoid.op((a: Int) => a * 2, endoMonoid.zero)(1) shouldBe 2
    endoMonoid.zero(3) shouldBe 3
  }

  test("foldMap") {
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).fold(m.zero)(m.op)

    def intSum: Monoid[Int] = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero: Int = 0
    }

    foldMap(List("a", "bb", "ccc"), intSum)(_.length) shouldBe 6
  }

}
