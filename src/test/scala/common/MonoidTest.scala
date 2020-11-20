package common

import answers.MonoidAnswers
import structures.Monoid

class MonoidTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("listConcat") {
    // given
    def listConcat[A] = MonoidAnswers.listConcat[A]

    // expect
    listConcat.op(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.op(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.op(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  test("intAddition") {
    //given
    def intAddition = MonoidAnswers.intAddition

    // expect
    intAddition.op(2, 3) shouldBe 5
    intAddition.op(intAddition.zero, 3) shouldBe 3
    intAddition.op(2, intAddition.zero) shouldBe 2
    intAddition.zero shouldBe 0
  }

  test("functionMonoid") {
    // given
    def intAddition = MonoidAnswers.intAddition
    def functionMonoid: Monoid[String => Int] = MonoidAnswers.functionMonoid(intAddition)

    // and
    def f1: String => Int = _.length
    def f2: String => Int = _.toInt

    // expect
    functionMonoid.op(f1, f2)("123") should be (3 + 123)
    functionMonoid.op(f1, f2)("1") should be (1 + 1)
  }

  test("bagMonoid") {
    // given
    def intAddition = MonoidAnswers.intAddition
    def mapMergeMonoid: Monoid[Map[String, Int]] = MonoidAnswers.mapMergeMonoid(intAddition)

    // and
    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("a" -> 2, "c" -> 3)

    // expect
    mapMergeMonoid.op(map1, map2) should be (Map("a" -> 3, "b" -> 2, "c" -> 3))
  }

}
