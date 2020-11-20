package answers.prepared

import structures.validation.{Name, PersonRequest, PhoneNumber, ValidPerson}
import structures.{Applicative, Failure, Success, Validation}

object PersonValidator {
  def validName(name: String): Validation[String, Name] =
    if (name != "") Success(Name(name))
    else Failure("Name cannot be empty")

  def validPhone(phoneNumber: String): Validation[String, PhoneNumber] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(PhoneNumber(phoneNumber))
    else Failure("Phone number must be 10 digits")

  def validate(person: PersonRequest): Validation[String, ValidPerson] = {
    Applicative.validationApplicative.map2(
      PersonValidator.validName(person.name),
      PersonValidator.validPhone(person.phoneNumber)
    )(ValidPerson)
  }
}
