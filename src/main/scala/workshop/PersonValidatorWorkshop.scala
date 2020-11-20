package workshop

import structures.validation.{Name, PersonRequest, PhoneNumber, ValidPerson}
import structures.{Applicative, Validation}

object PersonValidatorWorkshop {
  def validate(person: PersonRequest): Validation[String, ValidPerson] = null
  // hint: Applicative.validation.map2, check, ValidPerson
}