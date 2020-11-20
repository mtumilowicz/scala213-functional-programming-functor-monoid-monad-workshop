package workshop

import org.scalatest.Ignore
import structures.validation.{PersonRequest, ValidPerson}
import structures.{Failure, Success, Validation}

@Ignore
class PersonValidatorWorkshopTest extends org.scalatest.FunSuite with org.scalatest.matchers.should.Matchers {

  val subject = PersonValidatorWorkshop

  test("valid request") {
    // given
    val personRequest = PersonRequest("John", "1234567890")

    // when
    val validation: Validation[String, ValidPerson] = subject.validate(personRequest)

    // expect
    validation match {
      case Success(a) =>
        a.name.raw should be("John")
        a.phoneNumber.raw
    }
  }

  test("invalid request") {
    // given
    val personRequest = PersonRequest("", "")

    // when
    val validation: Validation[String, ValidPerson] = subject.validate(personRequest)

    // expect
    validation match {
      case Failure(head, tail) =>
        head should be ("Name cannot be empty")
        tail should be (Vector("Phone number must be 10 digits"))
    }
  }

}
