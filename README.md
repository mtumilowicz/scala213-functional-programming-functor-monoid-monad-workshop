# scala213-functional-programming-functor-monoid-monad-workshop
* references
    * http://blog.higher-order.com/assets/fpiscompanion.pdf
    * https://typelevel.org/cats/typeclasses/functor.html
    * https://typelevel.org/cats/typeclasses/monad.html
    * https://stackoverflow.com/questions/14598990/confused-with-the-for-comprehension-to-flatmap-map-transformation
    * https://docs.scala-lang.org/tutorials/FAQ/yield.html

* workshops order
    * Functor: distribute, list functor, option functor
    
## preface    
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
      * For example, if we distribute a
      List[(A, B)] , we get two lists of the same length, one with all the A s and the other
      with all the B s. That operation is sometimes called unzip. So we just wrote a generic
      unzip function that works not just for lists, but for any functor!
      * If the input to
        distribute is a list of pairs, the returned pair of lists will be of the same length as the
        input, and corresponding elements will appear in the same order
        * This kind of algebraic reasoning can potentially save us a lot of work, since we don’t have 
        to write separate tests for these properties
      
## monoids
* monoid consists of the following:
    * some type A
    * monoid laws (semigroup with an identity element)
        * associativity
            ```
            op(op(x,y), z) == op(x, op(y,z)) for any choice of x: A, y: A, z: A
            ```
        * identity
            ```
            exists zero: A, that op(x, zero) == x and op(zero, x) == x for any x: A
            ```
* trait
    ```
    trait Monoid[A] {
      def op(a1: A, a2: A): A
    
      def zero: A
    }
    ```
* example
    * string monoid
        ```
        val stringMonoid = new Monoid[String] {
            def op(a1: String, a2: String) = a1 + a2
            val zero = ""
        }
        ```
* suppose we have
    ```
    def foldRight[B](z: B)(f: (A, B) => B): B
    ```
    and we want to fold into the same type so
    ```
    def foldRight[A](z: A)(f: (A, A) => A): A
    ```
    monoid fit these argument types like a glove
    ```
    foldRight(monoid.zero)(monoid.op)
    ```
    note that foldLeft and foldRight when folding with a monoid gives the same results 
    due to associativity
* parallelism
    * note that due to associativity, below three operations give the same result 
        ```
        op(a, op(b, op(c, d)))
        op(op(op(a, b), c), d)
        op(op(a, b), op(c, d)) // allows for parallelism
        ```
    * proof
        ```
        op(op(a, b), op(c, d)) -> op(op(op(a, b), c), d)
        ```
* real power of monoids comes from the fact that they compose
    * consequence: if types A and B are monoids, then the tuple type (A, B) is also a monoid
    * means that we can perform multiple calculations simultaneously when folding a data structure
        * example: take the length and sum of a list at the same time in order to calculate

# functors
* code
    ```
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }
    ```
* we say that a type constructor F is a functor, and the Functor[F] instance constitutes proof 
that F is in fact a functor
* must obey two laws:
    * `fa.map(f).map(g) = fa.map(f.andThen(g))`
    * `fa.map(x => x) = fa`
* functors compose: https://github.com/mtumilowicz/scala212-cats-category-theory-composing-functors
* ref
    * https://github.com/mtumilowicz/java11-category-theory-optional-is-not-functor

# monads
* map can be implemented in terms of `flatMap` and `unit`
    ```
    def unit[A](a: => A): F[A]
    def map[A,B](f: A => B): Gen[B] = flatMap(a => unit(f(a)))
    ```
* they all have unit and flatMap , and each monad brings its own set of additional primitive 
operations that are specific to it
* you may be used to thinking of interfaces as providing a relatively complete API for an abstract 
data type, merely abstracting over the specific representation
    * example: List - LinkedList, ArrayList
* monad doesn’t generalize one type or another - rather many vastly different data types can satisfy the 
Monad interface and laws
    * Monad, like Monoid, is an abstract, purely algebraic interface
* monad is an implementation of one of the minimal sets of primitive combinators
satisfying the monad laws
* combinator sets
    * unit and flatMap
        ```
        def unit[A](a: => A): F[A]
        def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
        ```
    * unit and compose
        ```
        def unit[A](a: => A): F[A]
        def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C]
        ```
    * unit, map and join
        ```
        def unit[A](a: => A): F[A]
        def map[A, B](fa: F[A])(f: A => B): F[B]
        def join[A](mma: F[F[A]]): F[A]      
        ```
* monad laws
    * left identity and right identity
        ```
        compose(f, unit) == f
        compose(unit, f) == f
        ```
        * similar to zero element in monoids
    * associative law for monads
        ```
        compose(compose(f, g), h) == compose(f, compose(g, h))
        ```
        * similar to associative law for monoids
* chain of flatMap calls is like an imperative program with statements 
that assign to variables
    * monad specifies what occurs at statement boundaries
    * example
        * Option monad - may return None at some point and effectively terminate the processing
    * example: identity monad
        ```
        case class Id[A](value: A) {
          def map[B](f: A => B): Id[B] = Id(f(value))
        
          def flatMap[B](f: A => Id[B]): Id[B] = f(value)
        }
        ```
        ```
        val id = Identity("Hello, ")
          .flatMap(a => Identity("monad!")
          .flatMap(b => Identity(a + b)))
          
        val id2 = for {
          a <- Identity("Hello, ")
          b <- Identity("monad!")
        } yield a + b
      
      
        val a = "Hello, "
        val b = "monad!"
        val ab = a + b
      
        // id = Identity(Hello, monad!)
        // id2 = Identity(Hello, monad!)
        // ab = "Hello, monad!"
        ```
        * simply variable substitution
        * monads provide a context for introducing and binding variables, and performing 
        variable substitution
* IO context
    ```
  
    ```
    * is a kind of least common denominator for expressing programs with external effects
* 13.2.2 Benefits and drawbacks of the simple IO type
* https://miklos-martin.github.io/learn/fp/2016/03/10/monad-laws-for-regular-developers.html
* unlike Functors and Applicatives, not all Monads compose

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
* The applicative laws
    * Left and right identity
        * map(v)(id) == v
          map(map(v)(g))(f) == map(v)(f compose g)
        * In other words, map2 of some fa: F[A] with unit preserves the structure of fa
          map2(unit(()), fa)((_,a) => a) == fa
          map2(fa, unit(()))((a,_) => a) == fa
    * Associativity
        * If we didn’t have
          this law, we’d need two versions of map3 , perhaps map3L and map3R , depending on the
          groupin
        * We can state the associativity law in terms of product
            * def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))
            * def assoc[A,B,C](p: (A,(B,C))): ((A,B), C) = p match { case (a, (b, c)) => ((a,b), c) }
            * product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)
* Naturality of product
    * When working with Applicative effects, we generally
      have the option of applying transformations before or after combining values with map2
      * naturality law states that it doesn’t matter; we get the same result either way
    * def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
      (i,i2) => (f(i), g(i2))
    * map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))
    
* higher kinded type

* for comprehension
    * each line in the expression using the `<-` symbol is translated to a flatMap call, except 
        * the last line is translated to a concluding `map` call
        * `x <- c; if cond` is translated to `c.filter(x => cond)`
    * flatMap / map
        ```
        for {
          bound <- list
          out <- f(bound)
        } yield out
      
        // is equivalent to
      
        list.flatMap { bound =>
          f(bound).map { out =>
            out
          }
        }
        ```
    * flatMap / map / filter
        ```
        for{
          sl <- l
          el <- sl
          if el > 0
        } yield el.toString.length
      
        // is equivalent to
      
        l.flatMap(sl => sl.filter(el => el > 0).map(el => el.toString.length))
        ```