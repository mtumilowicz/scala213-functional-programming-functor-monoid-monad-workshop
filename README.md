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
    * There are monadic combinators such as join and flatMap that can’t
      be implemented with just map2 and unit
    * def join[A](f: F[F[A]]): F[A]
    * join function “removes a layer” of F
    * unit func-
      tion only lets us add an F layer, and map2 lets us apply a function within F but does no
      flattening of layers
    * Monad is clearly adding some extra capabilities beyond Applicative
    * Option applicative versus the Option monad
        * Suppose we’re using Option to work with the results of lookups in two Map objects
        * If
          we simply need to combine the results from two (independent) lookups, map2 is fine
            ```
            val F: Applicative[Option] = ...
            val departments: Map[String,String] = ...
            val salaries: Map[String,Double] = ...
            val o: Option[String] = F.map2(departments.get("Alice"), salaries.get("Alice")) {
                (dept, salary) => s"Alice in $dept makes $salary per year"
            }
            ```
        * If we want the result of one lookup to affect what lookup we do next, then we need flatMap or join 
            * we’re doing two lookups, but they’re independent and we merely want to com-
            bine their results within the Option context
            ```
            val idsByName: Map[String,Int]
            val departments: Map[Int,String] = ...
          
            val o: Option[String] = idsByName.get("Bob")
                .flatMap { id => departments.get(id) }
            }
            ```
    * We might say that with
      Applicative , the structure of our computation is fixed; with Monad , the results of pre-
      vious computations may influence what computations to run next
    * Applicative computations have fixed structure and simply sequence effects,
    whereas monadic computations may choose structure dynamically, based on
    the result of previous effects.
    * Applicative constructs context-free computations, while Monad allows for context
    sensitivity.
        * For example, a monadic parser allows for context-sensitive grammars while an applicative parser can only han-
          dle context-free grammars
* advantages of applicative functors
    * In general, it’s preferable to implement combinators like traverse using as few
      assumptions as possible. It’s better to assume that a data type can provide map2
      than flatMap . Otherwise we’d have to write a new traverse every time we
      encountered a type that’s Applicative but not a Monad 
    * Because Applicative is “weaker” than Monad , this gives the interpreter of applica-
      tive effects more flexibility. To take just one example, consider parsing. If we
      describe a parser without resorting to flatMap , this implies that the structure of
      our grammar is determined before we begin parsing. Therefore, our inter-
      preter or runner of parsers has more information about what it’ll be doing up
      front and is free to make additional assumptions and possibly use a more effi-
      cient implementation strategy for running the parser, based on this known
      structure. Adding flatMap is powerful, but it means we’re generating our pars-
      ers dynamically, so the interpreter may be more limited in what it can do.
    * Applicative functors compose, whereas monads (in general) don’t
* Not all applicative functors are monads
    * VALIDATION: AN EITHER VARIANT THAT ACCUMULATES ERRORS
        * Either data type and considered the question of how such a data type would have to be 
        modified to allow us to report multiple errors
            * For a concrete example, think of validating a web form submission
                * Only reporting the first error means the user would have to repeatedly submit 
                the form and fix one error at a time
        * def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f]
            * consider what happens in a sequence of flatMap calls like the following
                ```
                validName(field1) flatMap (f1 =>
                    validBirthdate(field2) flatMap (f2 =>
                        validPhone(field3) map (f3 => WebForm(f1, f2, f3))
                ```
            * If validName fails with an error, then validBirthdate and validPhone won’t even
              run. 
              * The computation with flatMap inherently establishes a linear chain of dependencies
            * Now think of doing the same thing with map3 :
              map3(
              validName(field1),
              validBirthdate(field2),
              validPhone(field3))(
              WebForm(_,_,_))
            * no dependency is implied between the three expressions passed to map3 , and in
              principle we can imagine collecting any errors from each Either into a List
                * if we
                  use the Either monad, its implementation of map3 in terms of flatMap will halt after
                  the first error
            
              