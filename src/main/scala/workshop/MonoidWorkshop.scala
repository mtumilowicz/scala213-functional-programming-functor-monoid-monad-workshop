package workshop

import structures.Monoid

object MonoidWorkshop {

  def listConcat[A]: Monoid[List[A]] = null // Monoid[List[A]]
  // hint: use ++ and Nil

  def intAddition: Monoid[Int] = null // new Monoid[Int]

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = {
    def merge(m1: Map[K, V], m2: Map[K, V])(combine: (V, V) => V): Map[K, V] = null
    // hint: sum sets and groupMapReduce

    // Monoid[Map[K, V]], hint: empty map and merge
    null
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = null // new Monoid[A => B]
}
