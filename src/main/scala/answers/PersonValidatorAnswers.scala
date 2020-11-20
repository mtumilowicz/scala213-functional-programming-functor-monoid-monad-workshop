package answers

import structures.validation.{Name, PersonRequest, PhoneNumber, ValidPerson}
import structures.{Applicative, Validation}

object PersonValidatorAnswers {
  def validate(person: PersonRequest): Validation[String, ValidPerson] = {
    Applicative.validation.map2(
      Name.check(person.name),
      PhoneNumber.check(person.phoneNumber)
    )(ValidPerson)
  }
}
