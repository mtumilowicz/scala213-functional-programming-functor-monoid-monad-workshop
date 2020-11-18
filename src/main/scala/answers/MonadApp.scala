package answers

object MonadApp extends App {

  val id = Identity("Hello, ")
    .flatMap(a => Identity("monad!")
    .flatMap(b => Identity(a + b)))
  println(id)

  val id2 = for {
    a <- Identity("Hello, ")
    b <- Identity("monad!")
  } yield a + b

  println(id2)

}
