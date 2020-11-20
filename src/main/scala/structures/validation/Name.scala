package structures.validation

case class Name(name: String) {
  require(name != null)
}