package common

import answers.prepared.MonoidAnswer

class MonoidTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("ListConcat") {
    def listConcat[A] = MonoidAnswer.listConcat[A]

    listConcat.op(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.op(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.op(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  test("intAddition") {
    def intAddition = MonoidAnswer.intAddition

    intAddition.op(2, 3) shouldBe 5
    intAddition.op(intAddition.zero, 3) shouldBe 3
    intAddition.op(2, intAddition.zero) shouldBe 2
    intAddition.zero shouldBe 0
  }

  test("IntMultiplication") {
    def intMultiplication = MonoidAnswer.intMultiplication

    intMultiplication.op(2, 3) shouldBe 6
    intMultiplication.op(intMultiplication.zero, 3) shouldBe 3
    intMultiplication.op(2, intMultiplication.zero) shouldBe 2
    intMultiplication.zero shouldBe 1
  }

  test("optionMonoid") {
    def optionMonoid[A] = MonoidAnswer.optionMonoid[A]

    optionMonoid.op(Some(1), Some(2)) shouldBe Some(1)
    optionMonoid.op(Some(1), Option.empty) shouldBe Some(1)
    optionMonoid.op(Option.empty, Some(1)) shouldBe Some(1)
    optionMonoid.zero shouldBe Option.empty
  }

  test("endoMonoid") {
    def endoMonoid[A] = MonoidAnswer.endoMonoid[A]

    endoMonoid.op((a: Int) => a * 2, (a: Int) => a * 3)(1) shouldBe 6
    endoMonoid.op(endoMonoid.zero, (a: Int) => a * 3)(1) shouldBe 3
    endoMonoid.op((a: Int) => a * 2, endoMonoid.zero)(1) shouldBe 2
    endoMonoid.zero(3) shouldBe 3
  }

  test("foldMap") {
    MonoidAnswer.foldMap(List("a", "bb", "ccc"), MonoidAnswer.intAddition)(_.length) shouldBe 6
  }

  test("foldRight") {
    MonoidAnswer.foldRight(List(1, 2, 3))("")(_ + _) shouldBe List(1, 2, 3).foldRight("")(_ + _)
  }

  test("functionMonoid") {
    def intMultiplication = MonoidAnswer.intMultiplication
    def functionMonoid: Monoid[String => Int] = MonoidAnswer.functionMonoid(intMultiplication)

    def f1: String => Int = _.length
    def f2: String => Int = _.toInt

    functionMonoid.op(f1, f2)("123") should be (3 * 123)
    functionMonoid.op(f1, f2)("1") should be (1 * 1)
  }

  test("bagMonoid") {
    def intAddition = MonoidAnswer.intAddition
    def mapMergeMonoid: Monoid[Map[String, Int]] = MonoidAnswer.mapMergeMonoid(intAddition)

    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("a" -> 2, "c" -> 3)
    mapMergeMonoid.op(map1, map2) should be (Map("a" -> 3, "b" -> 2, "c" -> 3))
  }

  test("take the length and sum of a list at the same time in order to calculate") {
    val intAddition = MonoidAnswer.intAddition
    val m = MonoidAnswer.productMonoid(intAddition, intAddition)

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
      as.map(f).fold(m.zero)(m.op)

    val p = foldMap(List(1,2,3,4), m)(a => (1, a))

    p should be (4, 10)
  }

}
