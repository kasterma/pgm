package net.kasterma.pgm

import scala.collection.immutable.IndexedSeq

class Values(val xs: List[Double]) {
  def size = xs.length
}

object Values {
  def apply(x: Double, xs: Double*): Values = new Values(List(x) ++ xs.toList)
  def apply(xs: List[Double]): Values = new Values(xs)
}

object Scope {
  def apply(name: String, x: Double, xs: Double*): Scope = new Scope(name, List(x) ++ xs.toList distinct)
  def apply(name: String, xs: List[Double]): Scope = new Scope(name, xs)
}

class Scope (val name: String, val xs: List[Double]) {
  def size = xs.length

  override def toString: String = "Scope(" + name + "," + xs.toString() + ")"
}

class AssignmentError(val message: String) extends Exception

object Assignment {
  def test_run = {
    val scopes = List(Scope("hi", 2.6, 1.5), Scope("ho", 2.3, 2.6))
    var x = Assignment(List(("hi", 2.6), ("ho", 2.3)), scopes)
    println(x.idx)
    x = Assignment(List(("hi", 1.5), ("ho", 2.3)), List(Scope("hi", 2.6, 1.5), Scope("ho", 2.3, 2.6)))
    println(x.idx)
    x = Assignment(List(("hi", 2.6), ("ho", 2.6)), List(Scope("hi", 2.6, 1.5), Scope("ho", 2.3, 2.6)))
    println(x.idx)
    x = Assignment(List(("hi", 1.5), ("ho", 2.6)), List(Scope("hi", 2.6, 1.5), Scope("ho", 2.3, 2.6)))
    println(x.idx)

  }

  def strides(scopes: List[Scope]): List[Int] = ((scopes dropRight 1) map ((l:Scope) => l.xs.length)).scan(1)(_ * _)

  def apply(assig: List[(String, Double)], scopes: List[Scope]): Assignment = {
    val assig_map = assig.toMap
    val assig_idx = scopes map (s => s.xs.indexOf(assig_map(s.name)))
    if (assig_idx exists (_ < 0)) {
      throw new AssignmentError("assignment misses a scope")
    }

    val index = assig_idx zip strides(scopes) map { case (x, stride) => x * stride} sum;
    new Assignment(assig_map, index)
  }

  def apply(index: Int, scopes: List[Scope]): Assignment = {
    val assig_idx = (scopes map (_.xs.length)) zip strides(scopes) map {case (len, stride) => index / stride % len }
    val assig_map = assig_idx zip scopes map { case (idx, scope) => (scope.name, scope.xs(idx))} toMap;
    new Assignment(assig_map, index)
  }
}

class Assignment (val assig_map: Map[String, Double], val index: Int) {
  def value(name: String) = assig_map(name)
  def idx = index
  override def toString: String = assig_map.toString()
}



class FactorError (message: String) extends Exception

class Factor (scopes: List[Scope], values: Values) {
  if ((scopes map (_.size) product) != values.size) {
    throw new FactorError("values column incorrect length")
  }

  def rows: List[List[Double]] = rows(scopes)

  def rows(scopes: List[Scope]): List[List[Double]] = {
    if (scopes.isEmpty) {
      List(List.empty)
    } else {
      val rem: List[List[Double]] = rows(scopes.tail)
      scopes.head.xs flatMap ((x: Double) => rem map ((xs: List[Double]) => x :: xs))
    }
  }

  def print(): Unit = {
    println()
    println("%s   vals".format(scopes map (_.name) mkString "   "))
    val rs = rows
    for (i <- 1 to rows.length) {
      println(rows(i-1) :+ values.xs(i-1) mkString " ")
    }
  }

  def evidence(var_name: String, value: Double): Factor = {
    val scope_idx: Int = scopes.indexWhere(x => x.name == var_name)
    val stride = scopes drop scope_idx map (_.size) product
    val stride2 = scopes drop (scope_idx + 1) map (_.size) product
    val within_scope_idx = scopes(scope_idx).xs.indexWhere(_.equals(value))
    val new_vals = values.xs.indices  filter (x => (x % stride) / stride2 == within_scope_idx) map {values.xs(_)} toList

    Factor(scopes.take(scope_idx) ++ scopes.drop(scope_idx + 1), Values(new_vals))
  }

  def value(as: Assignment): Double = {
    42.0
  }

  def update(as: Assignment, value: Double): Double = {
    value
  }
}

object Factor {
  def apply(xs: Scope*)(vals: Values): Factor = new Factor(xs.toList, vals)
  def apply(xs: List[Scope], vals: Values): Factor = new Factor(xs, vals)
  def apply(name: String, xs: Double*): FactorBuilder = FactorBuilder(name, xs)
}

class FactorBuilder (scopes: List[Scope]) {
  def apply(name: String, xs: Double*): FactorBuilder = new FactorBuilder(scopes :+ Scope(name, xs.toList))
  def apply(xs: Double*): Factor = Factor(scopes, Values(xs.toList))
}

object FactorBuilder {
  def apply(name: String, x: Double, xs: Double*) = new FactorBuilder(List(Scope(name, List(x) ++ xs.toList)))
  def apply(name: String, xs: Seq[Double]) = new FactorBuilder(List(Scope(name, xs.toList)))
}


/// Factor("x", 1, 2, 3)("y", 4,5)(1,2,3,4,5,6)


/// import net.kasterma.pgm._
/// val ff = Factor("x", 1, 2, 3)("y", 4,5)(1,2,3,4,5,6)
/// ff.rows


/// import net.kasterma.pgm._
/// val ff = Factor("x", 1, 2, 3)("y", 4,5)("z", 11,22)(1,2,3,4,5,6,11,22,33,44,55,66)
/// ff.rows