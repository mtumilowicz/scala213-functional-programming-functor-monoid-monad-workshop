package answers

class FunctorAnswersTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  val subject = FunctorAnswers

  test("list as a functor") {
    // given
    val listFunctor = subject.listFunctor

    // when
    val doubled = listFunctor.map(List(1, 2, 3))(_ * 2)
    val strings = listFunctor.map(doubled)(_.toString)

    // then
    strings shouldBe List("2", "4", "6")
  }

  test("option as a functor") {
    // given
    val optionFunctor = subject.optionFunctor

    // when
    val doubled = optionFunctor.map(Some(3))(_ * 2)
    val string = optionFunctor.map(doubled)(_.toString)

    // then
    string shouldBe Some("6")
  }

  test("unzip as a distribute in list functor") {
    // given
    val listFunctor = subject.listFunctor

    // when
    val unzipped = subject.distribute(listFunctor, List((1, "1"), (2, "2"), (3, "3")))

    // then
    unzipped shouldBe(List(1, 2, 3), List("1", "2", "3"))
  }
}
