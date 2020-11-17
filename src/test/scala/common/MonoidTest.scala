package common

import answers.MonoidsAnswer

class MonoidTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("ListConcat") {
    def listConcat[A] = MonoidsAnswer.listConcat[A]

    listConcat.op(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.op(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.op(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  test("intAddition") {
    def intAddition = MonoidsAnswer.intAddition

    intAddition.op(2, 3) shouldBe 5
    intAddition.op(intAddition.zero, 3) shouldBe 3
    intAddition.op(2, intAddition.zero) shouldBe 2
    intAddition.zero shouldBe 0
  }

  test("IntMultiplication") {
    def intMultiplication = MonoidsAnswer.intMultiplication

    intMultiplication.op(2, 3) shouldBe 6
    intMultiplication.op(intMultiplication.zero, 3) shouldBe 3
    intMultiplication.op(2, intMultiplication.zero) shouldBe 2
    intMultiplication.zero shouldBe 1
  }

  test("optionMonoid") {
    def optionMonoid[A] = MonoidsAnswer.optionMonoid[A]

    optionMonoid.op(Some(1), Some(2)) shouldBe Some(1)
    optionMonoid.op(Some(1), Option.empty) shouldBe Some(1)
    optionMonoid.op(Option.empty, Some(1)) shouldBe Some(1)
    optionMonoid.zero shouldBe Option.empty
  }

  test("endoMonoid") {
    def endoMonoid[A] = MonoidsAnswer.endoMonoid[A]

    endoMonoid.op((a: Int) => a * 2, (a: Int) => a * 3)(1) shouldBe 6
    endoMonoid.op(endoMonoid.zero, (a: Int) => a * 3)(1) shouldBe 3
    endoMonoid.op((a: Int) => a * 2, endoMonoid.zero)(1) shouldBe 2
    endoMonoid.zero(3) shouldBe 3
  }

  test("foldMap") {
    MonoidsAnswer.foldMap(List("a", "bb", "ccc"), MonoidsAnswer.intAddition)(_.length) shouldBe 6
  }

  test("foldRight") {
    MonoidsAnswer.foldRight(List(1, 2, 3))("")(_ + _) shouldBe List(1, 2, 3).foldRight("")(_ + _)
  }

  test("functionMonoid") {
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

      override def zero: A => B = _ => B.zero
    }
  }

  test("bagMonoid") {
    def merge[K, V](m1: Map[K, V], m2: Map[K, V])(combine: (V, V) => V): Map[K, V] = {
      val entries = m1.toSeq ++ m2.toSeq
      entries.groupMapReduce(_._1)(_._2)(combine)
    }

    def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
      new Monoid[Map[K, V]] {
        def zero: Map[K, V] = Map[K, V]()

        def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = merge(a, b)(V.op)
      }
  }

  test("take the length and sum of a list at the same time in order to calculate") {
    val intAddition = MonoidsAnswer.intAddition
    val m = MonoidsAnswer.productMonoid(intAddition, intAddition)

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).fold(m.zero)(m.op)

    val p = foldMap(List(1,2,3,4), m)(a => (1, a))

    p should be (4, 10)
  }

}
