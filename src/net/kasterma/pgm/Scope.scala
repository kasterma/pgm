package net.kasterma.pgm

/**
  * Created by kasterma on 13/09/16.
  */
class Scope[T] {

}

class FiniteSetScope[T](val pts: Set[T]) extends Scope[T] {
  def union(other: Scope[T]): Scope[T] = {
    other match {
      case other:FiniteSetScope[T] => new FiniteSetScope[T] (pts.union (other.pts) )
    }
  }

  def member(x: T): Boolean = pts.contains(x)
}

class FullTypeScope[T] extends Scope[T] {
  def union(other: Scope[T]): Scope[T] = this
  def member(x: T): Boolean = true
}

class PredicateScope[T](val pred: T => Boolean) extends Scope[T] {
  def member = pred
  def union(other: Scope[T]): Scope[T] = {
    other match {
      case other: PredicateScope[T] => new PredicateScope[T](x => other.member(x) | this.member(x))
      case other: FiniteSetScope[T] => new PredicateScope[T](x => this.member(x))
    }
  }
}

//class FinitePredicateScope[T](val pred: T => Boolean, val lower_bound: T, val upper_bound: T) extends Scope[T] {
//  def member = pred
//  def all_elements = (lower_bound to upper_bound) filter pred
//  def union(other: Scope[T]): Scope[T] = {
//    other match {
//      case other: FiniteSetScope[T] = new FiniteSetScope[T](other.pts.union(this.all_elements))
//    }
//  }
//}