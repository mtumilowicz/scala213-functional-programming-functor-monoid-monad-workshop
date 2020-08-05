package answers

object MonadApp extends App {

  val id = Id("Hello, ") flatMap (a => Id("monad!") flatMap (b => Id(a + b)))
  println(id)

  val id2 = for {
    a <- Id("Hello, ")
    b <- Id("monad!")
  } yield a + b

  println(id2)

}
