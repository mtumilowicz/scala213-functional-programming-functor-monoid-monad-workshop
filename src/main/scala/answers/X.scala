package answers

import java.time.LocalDate

import common.{Failure, Success, Validation}

object X {
  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")
}
