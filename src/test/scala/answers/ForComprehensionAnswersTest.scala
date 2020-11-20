package answers

class ForComprehensionAnswersTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  val subject = ForComprehensionAnswers

  test("even combinations") {
    // given
    val ints = List.range(1, 3)
    val chars = List.range('a', 'd')
    val strings = List("ff", "gg", "hh")

    // when
    val combinations = subject.evenCombinations(ints, chars, strings)

    // then
    combinations should be (List(
      "2 a ff", "2 a gg", "2 a hh",
      "2 b ff", "2 b gg", "2 b hh",
      "2 c ff", "2 c gg", "2 c hh")
    )
  }

}
