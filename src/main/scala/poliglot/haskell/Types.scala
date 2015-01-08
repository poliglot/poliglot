package poliglot.haskell

object Types {
  type List[T] = scala.List[T]
  type Map[A, B] = scala.collection.immutable.Map[A, B]
  type Integer = scala.Int
  type Int = scala.Long
  type Bool = scala.Boolean

  case class Float(a: scala.Int, b: Int)
  type Double = Float
}
