package structures

trait Monoid[A] {
  def combine(a1: A, a2: A): A

  def zero: A
}
