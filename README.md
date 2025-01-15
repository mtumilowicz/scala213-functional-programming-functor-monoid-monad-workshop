[![Build Status](https://app.travis-ci.com/mtumilowicz/scala213-functional-programming-functor-monoid-monad-workshop.svg?branch=master)](https://travis-ci.com/mtumilowicz/scala213-functional-programming-functor-monoid-monad-workshop)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# scala213-functional-programming-functor-monoid-monad-workshop
* references
    * http://blog.higher-order.com/assets/fpiscompanion.pdf
    * https://typelevel.org/cats/typeclasses/functor.html
    * https://typelevel.org/cats/typeclasses/monad.html
    * https://stackoverflow.com/questions/14598990/confused-with-the-for-comprehension-to-flatmap-map-transformation
    * https://docs.scala-lang.org/tutorials/FAQ/yield.html
    * https://www.james-willett.com/scala-map-flatmap-filter/
    * https://miklos-martin.github.io/learn/fp/2016/03/10/monad-laws-for-regular-developers.html
    * https://typelevel.org/blog/2016/08/21/hkts-moving-forward.html
    * https://netvl.github.io/scala-guidelines/type-system/higher-kinded-types.html
    * https://dzone.com/articles/application-type-lambdas-scala-0
    * https://carlo-hamalainen.net/2014/01/02/applicatives-compose-monads-do-not/
    * https://github.com/kitlangton/zio-from-scatch
    * https://www.manning.com/books/functional-programming-in-scala-second-edition
    * https://github.com/fpinscala/fpinscala/wiki
    * [Scala with Cats Book - Noel Welsh](https://underscore.io/books/scala-with-cats/)
    * [Functional Programming in Scala - Paul Chiusano](https://www.manning.com/books/functional-programming-in-scala)
    * [ZIO from Scratch — Part 1](https://www.youtube.com/watch?v=wsTIcHxJMeQ)
    * [ZIO from Scratch — Part 2](https://www.youtube.com/watch?v=g8Tuqldu2AE)
    * [Software Transactional Memory](https://www.youtube.com/watch?v=bLfxaHIvHfc)
    * [A Pragmatic Introduction to Category Theory—Daniela Sfregola](https://www.youtube.com/watch?v=Ss149MsZluI)
    * https://chat.openai.com/

## preface
* goals of this workshop
    * introduction to scala features:
        * for-comprehension
        * higher kinded types
    * developing basic intuitions concerning standard functional structures
        * monoid
        * functor and applicative functor
        * monad
    * show fundamental differences between above constructs
    * apply knowledge to real life
        * functional approach to validation
        * functional approach to IO
* workshops order
    1. `ForComprehensionWorkshop`
    1. `MonoidWorkshop`
    1. `FunctorWorkshop`
    1. `ApplicativeWorkshop`
    1. `PersonValidatorWorkshop`
    1. `MonadWorkshop`
    1. `EchoWorkshop`
    
## introduction    
* whenever we create an abstraction like Functor we should
    * consider abstract methods it should have
    * and laws we expect to hold for the implementations
        * of course Scala won’t enforce any of these laws
* laws are important for two reasons
    1. help an interface form a new semantic level
        * algebra may be reasoned about independently of the instances
        * example
            * we could proof that `Monoid[(A,B)]` constructed from `Monoid[A]` and `Monoid[B]`
            is actually a monoid
                * we don’t need to know anything about `A` and `B` to conclude this
    1. we often rely on laws when writing various combinators
        * example
            * unzip `List[(A, B)]` into `List[A]`, `List[B]`
                * same length
                * corresponding elements in the same order
            * algebraic reasoning can potentially save us a lot of work, since 
            we don’t have to write separate tests for these properties
                * we could write a generic unzip function that works for any functor
      
## monoids
* monoid consists of the following:
    * trait
        ```
        trait Monoid[A] {
          def combine(a1: A, a2: A): A
        
          def zero: A
        }
        ```
    * laws (semigroup with an identity element)
        * associativity
            ```
            combine(combine(x,y), z) == combine(x, combine(y,z)) for any choice of x: A, y: A, z: A
            ```
        * identity
            ```
            exists zero: A, that combine(x, zero) == x and combine(zero, x) == x for any x: A
            ```
    * is a type A and an implementation of `Monoid[A]` that satisfies the laws
* example
    * string monoid
        ```
        val stringMonoid = new Monoid[String] {
            def combine(a1: String, a2: String) = a1 + a2
            val zero = ""
        }
        ```
    * various Monoid instances don’t have much to do with each other
        * monoid is a type, together with the monoid operations and a set of laws
        * you may build some intuition by considering the various concrete instances
            * but nothing guarantees all monoids you encounter will match your intuition
    * we can say that
        * type A forms a monoid
        * type A is monoidal
        * less precisely: type A is a monoid or even type A is monoidal
        * but not: type A has monoid
            * analogy: the page you’re reading forms a rectangle, is rectangular or it is a rectangle
            but not: it has a rectangle
        * in any case, the Monoid[A] instance is simply evidence of this fact
* folding context
    ```
    def foldRight[B](z: B)(f: (A, B) => B): B
    ```
    and we want to fold into the same type so
    ```
    def foldRight[A](z: A)(f: (A, A) => A): A
    ```
    monoid fit these argument types like a glove
    ```
    foldRight(monoid.zero)(monoid.combine)
    ```
    `foldLeft` and `foldRight` gives the same results when folding with monoid (associativity) 
* parallelism
    * below three operations give the same result
        ```
        combine(a, combine(b, combine(c, d)))
        combine(combine(combine(a, b), c), d)
        combine(combine(a, b), combine(c, d)) // allows for parallelism
        ```
    * proof (associativity)
        ```
        combine(combine(combine(a, b), c), d) = combine(combine(x, y), z), where x = combine(a, b), y = c, z = d
        combine(combine(x, y), z) = combine(x, combine(y, z))
        combine(x, combine(y, z)) = combine(combine(a, b), combine(c, z))
        so: combine(combine(combine(a, b), c), d) = combine(combine(a, b), combine(c, d))
        ```
* additional properties
    * monoids compose
        * example: `A, B monoids -> (A, B) is also a monoid`
    * monoid homomorphism
        * condition: `A, B monoids => A.combine(f(x), f(y)) == f(B.combine(x, y))`
    * monoid isomorphism
        * homomorphism in both directions
        * example: `String` and `List[Char]` monoids with concatenation
    * commutative combine
        * when `combine(x, y) == combine(y, x))`
* semigroup
    * semigroup for a type A has an associative combine operation that returns an A given two input A values
        ```
        trait Semigroup[A] {
          def combine(a1: A, a2: A): A
        }
        ```
    * example: `NonEmptyList[A]`
        * there is no instance `Monoid[NonEmptyList[A]]` - you cannot specify `zero` element
## functors
* definition
    * we say that a type constructor `F` is a functor, and the `Functor[F]` instance constitutes proof 
    that `F` is in fact a functor
    ```
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A => B): F[B]
    }
    ```
* must obey two laws:
    * `fa.map(f).map(g) = fa.map(f.andThen(g))`
    * `fa.map(x => x) = fa`
* functors compose: https://github.com/mtumilowicz/scala212-cats-category-theory-composing-functors
* for more formal reasoning, please refer: https://github.com/mtumilowicz/java11-category-theory-optional-is-not-functor

## applicative functors
* definition
    ```
    trait Applicative[F[_]] {
    
      def unit[A](a: => A): F[A]
    
      def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    }
    ```
    * could be formulated using `unit` and `apply` (therefore called Applicative), rather than 
    `unit` and `map2`
        * `def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]`
* applicative laws
    * left and right identity
        ```
        map(v)(id) == v
        map(map(v)(g))(f) == map(v)(f compose g)
        ```
        in other words
        ```
        map2(unit(()), fa)((_,a) => a) == fa
        map2(fa, unit(()))((a,_) => a) == fa     
        ```
    * associativity (in terms of product)
         * `def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))`
         * `def assoc[A,B,C](p: (A,(B,C))): ((A,B), C) = p match { case (a, (b, c)) => ((a,b), c) }`
         * product(product(fa,fb),fc) == map(product(fa, product(fb,fc)))(assoc)
    * naturality law
        * `fa.map2(fb)((a, b) => (f(a), g(b))) == fa.map(f).product(fb.map(g))`
* all applicatives are functors
* advantages
    * preferable to implement combinators using as few assumptions as possible
        * it’s better to assume that a data type can provide `map2` than `flatMap`
        * otherwise we’d have to write a new `traverse` every time we encountered 
        a type that’s Applicative but not a Monad 
* validation context
    * only reporting the first error means the user would have to repeatedly submit 
    the form and fix one error at a time
    * consider what happens in a sequence of `flatMap` calls like the following
        ```
        validName(field1) flatMap (f1 =>
            validBirthdate(field2) flatMap (f2 =>
                validPhone(field3) map (f3 => ValidInput(f1, f2, f3))
        ```
        * if `validName` fails with an error, then `validBirthdate` and `validPhone` won’t even run 
    * now think of doing the same thing with `map3`
        ```
        map3(validName(field1), validBirthdate(field2), validPhone(field3))(ValidInput(_,_,_))
        ```
        * no dependency implied between the three expressions
* additional properties
    * large number of the useful combinators on Monad can be defined using only unit and map2
        * example
            ```
            def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
                fa.flatMap(a => fb.map(b => f(a, b)))
            ```
    * applicative functors compose
        * `F[_]` and `G[_]` are applicative functors => `F[G[_]]` is applicative functor
## monads
* definition
    ```
    trait Monad[F[_]] {
    
      def unit[A](a: => A): F[A]
    
      def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    
    }
    ```
* monad laws
    * given compose function
        ```
        def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
            a => flatMap(f(a))(g)
        ```
        * digression
            * functions like that: `A => F[B]` are called Kleisli arrows
    * left identity and right identity
        ```
        compose(f, unit) == f // m.flatMap(unit) == m
        compose(unit, f) == f // unit(a).flatMap(func) == func(a)
      
        // example for option
        option.flatMap(Some(_)) == option
      
        Some(value).flatMap(f) == f(value)
        ```
        * similar to zero element in monoids
    * associative law for monads
        ```
        compose(compose(f, g), h) == compose(f, compose(g, h))
      
        // example for option
        option.flatMap(f).flatMap(g) == option.flatMap(f(_).flatMap(g))
        ```
        * similar to associative law for monoids
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
* each monad brings its own set of additional primitive operations that are specific to it
    * example: `Option`, `Either`, `List`
* vs interfaces
    * interfaces ~ relatively complete API for an abstract data type
        * merely abstracting over the specific representation
        * example: List - LinkedList, ArrayList
    * monad ~ many vastly different data types can satisfy the Monad interface and laws
        * like Monoid, is an abstract, purely algebraic interface
* chain of `flatMap` calls is like an imperative program with statements 
that assign to variables
    * monad specifies what occurs at statement boundaries
    * example
        * `Option` monad - may return None at some point and effectively terminate the processing
    * example: identity monad
        ```
        case class Id[A](value: A) {
          def map[B](f: A => B): Id[B] = Id(f(value))
        
          def flatMap[B](f: A => Id[B]): Id[B] = f(value)
        }
        ```
        ```
        val id = Id("Hello, ")
          .flatMap(a => Id("monad!")
          .flatMap(b => Id(a + b)))
      
        // id = Id(Hello, monad!)
        ```
        * simply variable substitution
            ```
            for {
                a <- Id("Hello, ")
                b <- Id("monad!")
            } yield a + b
            ```
            vs
            ```
            val a = "Hello, "
            val b = "monad!"
            val c = a + b
            ```
            * variables `a` and `b` get bound to `"Hello, "` and `"monad!”`
        * monads provide a context for introducing and binding variables, and performing 
        variable substitution
* unlike Functors and Applicatives, not all Monads compose
    * to show that "monads do not compose", it is sufficient to find a counterexample, namely two monads 
    `f` and `g` such that `f g` is not a monad
    * proof: https://carlo-hamalainen.net/2014/01/02/applicatives-compose-monads-do-not/
    * problem
        * to implement `join` for nested monads `F` and `G`, write something of a type like:
            ```
            F[G[F[G[A]]]] => F[G[A]]
            ```
        * that can’t be written generally
        * but if `G` also happens to have a `Traverse` instance
            ```
            F[F[G[G[A]]]] => F[G[A]] // use sequence to turn G[F[_]] into F[G[_]], then use join on F then on G
            ```
    * problem is often addressed with a custom-written version specifically constructed for composition
        * example: monad transformers like `OptionT`
* all monads are applicatives
    * not all applicatives are monads
    * difference between monads and applicatives
        * applicative constructs context-free computations, while Monad allows for context sensitivity
        * `join` and `flatMap` can’t be implemented with just `map2` and `unit`
            * `def join[A](f: F[F[A]]): F[A] // removes a layer of F`
            * `unit` function only lets us add an `F` layer
            * `map2` lets us apply a function within `F` but does no flattening of layers
    * `Option` applicative versus the `Option` monad
        * combine the results from two (independent) lookups: `map2`
            ```
            val F: Applicative[Option] = ...
            val departments: Map[String,String] = ...
            val salaries: Map[String,Double] = ...
            val o: Option[String] = F.map2(departments.get("Alice"), salaries.get("Alice")) {
                (dept, salary) => s"Alice in $dept makes $salary per year"
            }
            ```
        * result of one lookup to affect next lookup: `flatMap` 
            ```
            val idsByName: Map[String,Int]
            val departments: Map[Int,String] = ...
          
            val o: Option[String] = idsByName.get("Bob")
                .flatMap { id => departments.get(id) }
            }
            ```
* each monad brings its own set of additional primitive operations that are specific to it
    * example: `State[S, A]`
        ```
        def get[S]: State[S, S]
        def set[S](s: => S): State[S, Unit]
        ```          
* monad is a monoid in a category of endofunctors
    * endofunctor - functor that maps a category to itself
    * let's look for minimal set of combinators for a monad: unit, map and join
        * monoid = unit + join
        * endofunctor = map
    * `Monoid[M]` - objects: Scala types, arrows: Scala functions
    * `Monad[F]` - objects: Scala functors, arrows: natural transformations
    * overview
        |             | `zero`/`unit`   | `op`/`join`|
        --------------|-----------------|------------|
        | `Monoid[M]` | `1 => M`        | `M² => M`  |
        | `Monad[F]`  | `1 ~> F`        | `F² ~> F`  |
        * `~>` is natural transformation
        * `M²` is `(M, M)`
        * `F²` is `F[F[A]]`
        * `1` is
            * for Monoid: `Unit` type
            * for Monad: identity functor

### IO context
```
sealed trait IO[A] {
  self => //  lets us refer to this object inside closures
  def unsafeRunSync: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run: B = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run: B = f(self.run).run
    }
}
```
* in short: `IO` is a magic thing that says this function depends on something other than its arguments 
* clearly separates pure code from impure code, forcing us to be honest about where 
interactions with the outside world are occurring
    * referentially transparent description of a computation with effects
* IO computations are ordinary values
    * we can store them in lists, pass them to functions, create them dynamically, and so on
    * example
        ```
        def PrintLine(msg: String): IO = new IO {
          def unsafeRun = println(msg)
        }

        def contest(p1: Player, p2: Player): IO = // pure function
          PrintLine(winnerMsg(winner(p1, p2)))
        ```
* given `IO[A]` will overflow the runtime call stack and throw a `StackOverflowError`
    * solution: encapsulate operations into dedicated case classes and use tail recursion for evaluation
        * `io.flatMap(f).flatMap(g) = io.flatMap(v => f(v) flatMap g)`
        * approach called trampoline
             
## appendix
### higher kinded type
* represent an ability to abstract over type constructors
* suppose we have same implementations, different type constructors
    ```
    def tuple[A, B](as: List[A], bs: List[B]): List[(A, B)] =
      as.flatMap{a =>
        bs.map((a, _))}
        
    def tuple[A, B](as: Option[A], bs: Option[B]): Option[(A, B)] =
      as.flatMap{a =>
        bs.map((a, _))}
        
    def tuple[E, A, B](as: Either[E, A], bs: Either[E, B]): Either[E, (A, B)] =
      as.flatMap{a =>
        bs.map((a, _))}
    ```
    * in programming, when we encounter such great sameness—not merely similar code, 
    but identical code—we would like the opportunity to parameterize: extract the parts 
    that are different to arguments, and recycle the common code for all situations
    * we have a way to pass in implementations; that’s just higher-order functions
    * we need "type constructor as argument"
        ```
        def tuplef[F[_], A, B](fa: F[A], fb: F[B]): F[(A, B)] = ???      
        ```
        * `F[_]` means that `F` may not be a simple type, like `Int` or `String`, but instead 
        a one-argument type constructor, like `List` or `Option`
* higher-kinded types are sometimes used in libraries
    * example: standard Scala collections
* Scala doesn’t allow us to use underscore syntax to simply say `State[Int, _]` to create 
an anonymous type constructor like we create anonymous functions
    * we have to go through type projections
        * `({type L[a] = Map[K, a]})#L`
            * declares an anonymous type
            * then access its `L` member with the `#` syntax
            * type constructor declared inline like this is often called a type lambda in Scala
    * removed in Scala 3
### for comprehension
* each line in the expression using the `<-` is translated to a `flatMap` call, except 
    * the last line (`yield`) - it is translated to a concluding `map` call
    * `x <- c if cond` is translated to `c.filter(x => cond)`
* `flatMap` / `map`
    ```
    for {
      bound <- list
      out <- f(bound)
    } yield out
  
    // is equivalent to (could be desugared with IntelliJ)
  
    list.flatMap { bound =>
      f(bound).map { out =>
        out
      }
    }
    ```
* `flatMap` / `map` / `filter`
    ```
    for {
      sl <- l
      el <- sl if el > 0
    } yield el.toString.length
  
    // is equivalent to
  
    l.flatMap(sl => sl.filter(el => el > 0).map(el => el.toString.length))
    ```
