package answers

import common.{Applicative, Validation}

object ValidateApp extends App {

  def validate(person: PersonRequest): Validation[String, ValidPerson] = {
    Applicative.validationApplicative.map2(
      PersonValidations.validName(person.name),
      PersonValidations.validPhone(person.phoneNumber)
    )(ValidPerson)
  }

  println(validate(PersonRequest("Abc", "1234567890")))
}