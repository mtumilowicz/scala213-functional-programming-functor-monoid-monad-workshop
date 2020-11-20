package common

import answers.FunctorAnswers

class FunctorTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("list as a functor") {
    val listFunctor = FunctorAnswers.listFunctor

    val doubled = listFunctor.map(List(1, 2, 3))(_ * 2)
    val strings = listFunctor.map(doubled)(_.toString)

    strings shouldBe List("2", "4", "6")
  }

  test("unzip as a distribute in list functor") {
    val listFunctor = FunctorAnswers.listFunctor

    val unzipped = FunctorAnswers.distribute(listFunctor, List((1, "1"), (2, "2"), (3, "3")))
    unzipped shouldBe(List(1, 2, 3), List("1", "2", "3"))
  }


  test("option as a functor") {
    val optionFunctor = FunctorAnswers.optionFunctor

    val doubled = optionFunctor.map(Some(3))(_ * 2)
    val string = optionFunctor.map(doubled)(_.toString)

    string shouldBe Some("6")
  }

}
