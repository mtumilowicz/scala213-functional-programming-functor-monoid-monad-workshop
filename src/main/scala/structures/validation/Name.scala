package structures.validation

import structures.{Failure, Success, Validation}

case class Name private(raw: String)

object Name {
  def check(name: String): Validation[String, Name] =
    if (name != "") Success(Name(name))
    else Failure("Name cannot be empty")
}