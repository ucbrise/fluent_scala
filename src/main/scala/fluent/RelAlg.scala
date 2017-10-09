package fluent

sealed trait RelAlg[A <: Product] {
  def filter(f: A => Boolean) = Filter(this, f)
  def map[B <: Product](f: A => B) = Map(this, f)
  def cross[B <: Product](rhs: RelAlg[B]) = Cross(this, rhs)
  def union(rhs: RelAlg[A]) = Union(this, rhs)
  def intersect(rhs: RelAlg[A]) = Intersect(this, rhs)
  def diff(rhs: RelAlg[A]) = Diff(this, rhs)
  def group[K, B <: Product](key: A => K, agg: Set[A] => B) = {
    Group(this, key, agg)
  }
  def equijoin[K, B <: Product](lkey: A => K, rhs: RelAlg[B], rkey: B => K) = {
    EquiJoin(this, lkey, rhs, rkey)
  }
}

case class Empty[A <: Product]() extends RelAlg[A]
case class Const[A <: Product](x: A) extends RelAlg[A]
case class Relation[A <: Product](t: Collection[A]) extends RelAlg[A]
case class Filter[A <: Product](q: RelAlg[A], f: A => Boolean) extends RelAlg[A]
case class Map[A <: Product, B <: Product](
  q: RelAlg[A], f: A => B) extends RelAlg[B]
case class Cross[A <: Product, B <: Product](
  lhs: RelAlg[A], rhs: RelAlg[B]) extends RelAlg[(A, B)]
case class Union[A <: Product](lhs: RelAlg[A], rhs: RelAlg[A]) extends RelAlg[A]
case class Intersect[A <: Product](
  lhs: RelAlg[A], rhs: RelAlg[A]) extends RelAlg[A]
case class Diff[A <: Product](lhs: RelAlg[A], rhs: RelAlg[A]) extends RelAlg[A]
case class Group[K, A <: Product, B <: Product](
  q: RelAlg[A], key: A => K, agg: Set[A] => B) extends RelAlg[B]
case class EquiJoin[K, A <: Product, B <: Product](
  lhs: RelAlg[A],
  lkey: A => K,
  rhs: RelAlg[B],
  rkey: B => K) extends RelAlg[(A, B)]

object RelAlg {
  def eval[A <: Product](q: RelAlg[A]): Set[A] = {
    q match {
      case Empty() => Set()
      case Const(x) => Set(x)
      case Relation(t) => t.get().toSet
      case Filter(q, f) => eval(q).filter(f)
      case Map(q, f) => eval(q).map(f)
      case Cross(lhs, rhs) => for (l <- eval(lhs); r <- eval(rhs)) yield (l, r)
      case Union(lhs, rhs) => eval(lhs).union(eval(rhs))
      case Intersect(lhs, rhs) => eval(lhs).intersect(eval(rhs))
      case Diff(lhs, rhs) => eval(lhs).diff(eval(rhs))
      case Group(q, key, ag) => eval(q).groupBy(key).mapValues(ag).values.toSet
      case EquiJoin(lhs, lkey, rhs, rkey) => {
        val groups = eval(lhs).groupBy(lkey)
        eval(rhs).flatMap(r => {
          groups.get(rkey(r)).getOrElse(Set()).map((_, r))
        })
      }
    }
  }
}
