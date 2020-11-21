package workshop

import org.scalatest.Ignore
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import structures.Applicative

@Ignore
class ApplicativeWorkshopTest extends org.scalatest.FunSuite {

  val subject = ApplicativeWorkshop

  test("test sequence") {
    // given
    val list = List(Some(1), Some(2), Some(3))
    val applicative = Applicative.optionApplicative()

    // when
    val sequenced = subject.sequence(applicative, list)
    val empty = subject.sequence(applicative, List(Some(1), None))

    // expect
    sequenced shouldBe Some(List(1, 2, 3))
    empty shouldBe None
  }

  test("test map") {
    // given
    val list = List(1, 2, 3)
    val applicative = Applicative.listApplicative()

    // when
    val mapped = subject.map(applicative, list)(_ * 2)

    // expect
    mapped shouldBe List(2, 4, 6)
  }

  test("test apply") {
    // given
    val some = Some(2)
    val none = Option.empty[Int]
    val applicative = Applicative.optionApplicative()

    // when
    val applied = subject.apply(applicative, Some((i: Int) => i.toString))(some)
    val appliedEmpty = subject.apply(applicative, Option.empty[Int => String])(some)
    val empty = subject.apply(applicative, Some((i: Int) => i * 2))(none)

    // expect
    applied shouldBe Some("2")
    appliedEmpty shouldBe None
    empty shouldBe None
  }

}
