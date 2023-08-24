package answers

import structures.Monoid

object MonoidAnswers {

  def listConcat[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override val zero: List[Nothing] = Nil
  }

  def intAddition: Monoid[Int] = new Monoid[Int] {
    override def combine(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = {
    def merge(m1: Map[K, V], m2: Map[K, V])(combine: (V, V) => V): Map[K, V] = {
      val entries = m1.toSeq ++ m2.toSeq
      entries.groupMapReduce(_._1)(_._2)(combine)
    }

    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K, V]()

      def combine(a: Map[K, V], b: Map[K, V]): Map[K, V] = merge(a, b)(V.combine)
    }
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def combine(a1: A => B, a2: A => B): A => B = a => B.combine(a1(a), a2(a))

    override def zero: A => B = _ => B.zero
  }
}
