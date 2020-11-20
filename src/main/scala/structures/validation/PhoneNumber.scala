package structures.validation

import structures.{Failure, Success, Validation}

case class PhoneNumber private(raw: String)

object PhoneNumber {
  def check(phoneNumber: String): Validation[String, PhoneNumber] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(PhoneNumber(phoneNumber))
    else Failure("Phone number must be 10 digits")
}