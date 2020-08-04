package answers

import common.{Applicative, Validation}

object ValidEx extends App {

  def validWebForm(name: String, phone: String): Validation[String, WebForm] =
    Applicative.validationApplicative.map2(
      X.validName(name),
      X.validPhone(phone)
    )(
      WebForm)


  println(validWebForm("Abc", "1234567890"))

}