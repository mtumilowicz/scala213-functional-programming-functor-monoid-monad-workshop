# scala213-functional-programming-functor-monoid-monad-workshop
* references
    * http://blog.higher-order.com/assets/fpiscompanion.pdf

* workshops order
    * Functor: distribute, list functor, option functor
    

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
* For example, if we distribute a
List[(A, B)] , we get two lists of the same length, one with all the A s and the other
with all the B s. That operation is sometimes called unzip. So we just wrote a generic
unzip function that works not just for lists, but for any functor!
* Whenever we create an abstraction like Functor , we should consider not only what
  abstract methods it should have, but which laws we expect to hold for the implementa-
  tions
  * of course Scala won’t enforce any of these laws
* laws are important for two reasons
    * Laws help an interface form a new semantic level whose algebra may be rea-
      soned about independently of the instances
        * example, when we take the prod-
          uct of a Monoid[A] and a Monoid[B] to form a Monoid[(A,B)] , the monoid laws
          let us conclude that the “fused” monoid operation is also associative
          * We don’t need to know anything about A and B to conclude this
    * we often rely on laws when writing various combinators
      derived from the functions of some abstract interface like Functor
      * example ???
* If the input to
  distribute is a list of pairs, the returned pair of lists will be of the same length as the
  input, and corresponding elements will appear in the same order
  * This kind of algebraic reasoning can potentially save us a lot of work, since we don’t have 
  to write separate tests for these properties

## monads
* we know that map can be implemented in terms of flatMap and unit
    * def map[A,B](f: A => B): Gen[B] = flatMap(a => unit(f(a)))
* Remember the associative law for monoids?
    * op(op(x,y), z) == op(x, op(y,z))
    * associative law for monads: compose(compose(f, g), h) == compose(f, compose(g, h))
* Just like zero was an identity element for
  append in a monoid, there’s an identity element for compose in a monad
  * exactly what unit is: def unit[A](a: => A): F[A]
  * form of two laws, left identity and right identity:
    * compose(f, unit) == f
    * compose(unit, f) == f
* You may be used to thinking of interfaces as providing a relatively complete API for
  an abstract data type, merely abstracting over the specific representation
  * After all, a
    singly linked list and an array-based list may be implemented differently behind the
    scenes, but they’ll share a common interface in terms of which a lot of useful and con-
    crete application code can be written
  * Monad , like Monoid , is a more abstract, purely
    algebraic interface
    * The Monad combinators are often just a small fragment of the full
      API for a given data type that happens to be a monad
    * So Monad doesn’t generalize one
      type or another; rather, many vastly different data types can satisfy the Monad interface
      and laws
  * three minimal sets of primitive Monad combinators
    * unit and flatMap
    * unit and compose
    * unit , map , and join 
  * A monad is an implementation of one of the minimal sets of monadic
    combinators, satisfying the laws of associativity and identity
* what is the meaning of the identity monad
     * what is the action of flatMap for the identity monad?
     * It’s simply variable substitution
     * variables a and b get bound to "Hello, " and "monad!" , respectively, and
       then substituted into the expression a + b
     * without the Id wrapper
        val a = "Hello, "
        val b = "monad!"
        val ab = a + b
     * We could say that monads provide a context for
       introducing and binding variables, and performing variable substitution
* This is true in general for monads—they all have unit and flatMap , and each monad
  brings its own set of additional primitive operations that are specific to it
* We can see that a chain of flatMap calls (or an equivalent
  for-comprehension) is like an imperative program with statements that assign to vari-
  ables, and the monad specifies what occurs at statement boundaries
  * For example, with Id ,
    nothing at all occurs except unwrapping and rewrapping in the Id constructor
  * With the
    Option monad, a statement may return None and terminate the program
  * With the
    List monad, a statement may return many results, which causes statements that follow
    it to potentially run multiple times, once for each result
* Monad contract doesn’t specify what is happening between the lines, only that
  whatever is happening satisfies the laws of associativity and identity
* Monads provide a powerful interface, as evidenced by the fact
  that we can use flatMap to essentially write imperative programs in a purely func-
  tional way
* We’ve seen three minimal sets of primitive Monad combinators, and instances of
Monad will have to provide implementations of one of these sets:
    * unit and flatMap
    * unit and compose
    * unit , map , and join
* 13.2.2 Benefits and drawbacks of the simple IO type

## monoids
* monoids
    * Associativity and parallelism
      
## applicative functors
* The name applicative comes from the fact that we can formulate the Applicative
  interface using an alternate set of primitives, unit and the function apply , rather than
  unit and map2
  
* all monads
  are applicative functors, and we don’t need to provide separate Applicative instances
  for all our data types that are already monads
  
* We might say that with
  Applicative , the structure of our computation is fixed; with Monad , the results of pre-
  vious computations may influence what computations to run next
* all applicatives are functors
* name applicative comes from the fact that we can formulate the Applicative
  interface using an alternate set of primitives, unit and the function apply , rather than
  unit and map2
  * def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
  * apply method is useful for implementing map3 , map4 , and so on
    def map3[A,B,C,D](fa: F[A],
    fb: F[B],
    fc: F[C])(f: (A, B, C) => D): F[D]
* Monad[F] a subtype of Applicative[F] by providing
  the default implementation of map2 in terms of flatMap . This tells us that all monads
  are applicative functors
* difference between monads and applicative functors
    
