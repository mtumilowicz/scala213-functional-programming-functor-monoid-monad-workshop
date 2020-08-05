package common

class FunctorTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  test("list as a functor") {
    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }

    val doubled = listFunctor.map(List(1, 2, 3))(_ * 2)
    val strings = listFunctor.map(doubled)(_.toString)

    strings shouldBe List("2", "4", "6")
  }

  test("unzip as a distribute in list functor") {
    val listFunctor = new Functor[List] {
      def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    }

    val unzipped = listFunctor.distribute(List((1, "1"), (2, "2"), (3, "3")))
    unzipped shouldBe(List(1, 2, 3), List("1", "2", "3"))
  }


  test("option as a functor") {
    val optionFunctor = new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f
    }

    val doubled = optionFunctor.map(Some(3))(_ * 2)
    val string = optionFunctor.map(doubled)(_.toString)

    string shouldBe Some("6")
  }

}
