package workshop

import org.scalatest.Ignore

@Ignore
class MonadWorkshopTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  val subject = MonadWorkshop

  test("option as a monad") {
    // given
    val maybe = subject.maybe

    // expect
    maybe.unit(1) shouldBe Some(1)
    maybe.flatMap(Some(2))(a => Some(a * 2)) shouldBe Some(4)
    maybe.flatMap(Option.empty[Int])(a => Some(a * 2)) shouldBe None
  }

}
