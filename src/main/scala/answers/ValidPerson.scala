package answers

case class PersonRequest(name: String, phoneNumber: String)

case class ValidPerson(name: String, phoneNumber: String)

case class Name(name: String) {
  require(name != null)
}

case class PhoneNumber(raw: String) {
  require(raw.matches("[0-9]{10}"))
}
