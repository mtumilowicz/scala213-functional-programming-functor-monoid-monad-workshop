package structures.validation

case class PhoneNumber(raw: String) {
  require(raw.matches("[0-9]{10}"))
}