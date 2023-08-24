package workshop

import org.scalatest.Ignore
import structures.Monoid

@Ignore
class MonoidWorkshopTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  val subject = MonoidWorkshop

  test("listConcat") {
    // given
    def listConcat[A] = subject.listConcat[A]

    // expect
    listConcat.combine(List("a"), List("b")) shouldBe List("a", "b")
    listConcat.combine(listConcat.zero, List("b")) shouldBe List("b")
    listConcat.combine(List("a"), listConcat.zero) shouldBe List("a")
    listConcat.zero shouldBe Nil
  }

  test("intAddition") {
    //given
    def intAddition = subject.intAddition

    // expect
    intAddition.combine(2, 3) shouldBe 5
    intAddition.combine(intAddition.zero, 3) shouldBe 3
    intAddition.combine(2, intAddition.zero) shouldBe 2
    intAddition.zero shouldBe 0
  }

  test("functionMonoid") {
    // given
    def intAddition = subject.intAddition
    def functionMonoid: Monoid[String => Int] = subject.functionMonoid(intAddition)

    // and
    def f1: String => Int = _.length
    def f2: String => Int = _.toInt

    // expect
    functionMonoid.combine(f1, f2)("123") should be (3 + 123)
    functionMonoid.combine(f1, f2)("1") should be (1 + 1)
  }

  test("bagMonoid") {
    // given
    def intAddition = subject.intAddition
    def mapMergeMonoid: Monoid[Map[String, Int]] = subject.mapMergeMonoid(intAddition)

    // and
    val map1 = Map("a" -> 1, "b" -> 2)
    val map2 = Map("a" -> 2, "c" -> 3)

    // expect
    mapMergeMonoid.combine(map1, map2) should be (Map("a" -> 3, "b" -> 2, "c" -> 3))
  }

}
