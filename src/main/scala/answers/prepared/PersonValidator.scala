package answers.prepared

import structures.validation.{Name, PersonRequest, PhoneNumber, ValidPerson}
import structures.{Applicative, Validation}

object PersonValidator {
  def validate(person: PersonRequest): Validation[String, ValidPerson] = {
    Applicative.validationApplicative.map2(
      Name.check(person.name),
      PhoneNumber.check(person.phoneNumber)
    )(ValidPerson)
  }
}
