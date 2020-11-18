package answers.prepared

object ForComprehensionAnswers {

  def allCombinations(ints: List[Int], chars: List[Char], strings: List[String]): List[String] = {
    for {
      i <- ints if i % 2 == 0
      c <- chars
      s <- strings
    } yield (i + " " + c + " " + s)
  }

}
