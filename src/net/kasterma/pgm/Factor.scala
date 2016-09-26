package net.kasterma.pgm

class Values[T](val xs: List[T]) {
  def size = xs.length
}

object Values {
  def apply[T](x: T, xs: T*): Values[T] = new Values(List(x) ++ xs.toList)
}

class Scope[+T] (val xs: List[T]) {
  def size = xs.length
 // def classname = T
}

object Scope {
  def apply[T](x: T, xs: T*): Scope[T] = new Scope(List(x) ++ xs.toList distinct)
}

class FactorError (message: String) extends Exception

class Factor[T] (scopes: List[Scope[Any]], values: Values[T]) {
  if ((scopes map (_.size) product) != values.size) {
    throw new FactorError("values column incorrect length")
  }

}
