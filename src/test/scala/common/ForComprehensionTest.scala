package common

import answers.ForComprehensionAnswers
import answers.prepared.FunctorAnswers

class ForComprehensionTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("allCombinations") {
    val ints = List.range(1, 3)
    val chars = List.range('a', 'd')
    val strings = List("ff", "gg", "hh")

    val combinations = ForComprehensionAnswers.allCombinations(ints, chars, strings)

    combinations should be (List("2 a ff", "2 a gg", "2 a hh", "2 b ff", "2 b gg", "2 b hh", "2 c ff", "2 c gg", "2 c hh"))
  }

}
