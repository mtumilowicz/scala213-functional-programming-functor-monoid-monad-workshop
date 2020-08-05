# scala213-functional-programming-functor-monoid-monad-workshop



## functors
* https://github.com/mtumilowicz/java11-category-theory-optional-is-not-functor
* we implemented a `map` function to lift a function taking one argument "into the context of" some 
data type
* Scala trait the idea of "a data type that implements map"
```
trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]
}
```


* monoids
    * Associativity and parallelism
    
    
We’ve seen three minimal sets of primitive Monad combinators, and instances of
Monad will have to provide implementations of one of these sets:
    * unit and flatMap
    * unit and compose
    * unit , map , and join
    
* difference between monads and applicative functors

* 13.2.2 Benefits and drawbacks of the simple IO type

* The name applicative comes from the fact that we can formulate the Applicative
  interface using an alternate set of primitives, unit and the function apply , rather than
  unit and map2
  
* all monads
  are applicative functors, and we don’t need to provide separate Applicative instances
  for all our data types that are already monads
  
* We might say that with
  Applicative , the structure of our computation is fixed; with Monad , the results of pre-
  vious computations may influence what computations to run next