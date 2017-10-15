package fluent

import scala.language.implicitConversions

case class SetUnionLattice[A](var xs: Set[A]) extends Lattice[SetUnionLattice[A]] {
  def this() = this(Set())

  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: SetUnionLattice[A]): Option[Int] = {
    (xs.subsetOf(that.xs), xs == that.xs, that.xs.subsetOf(xs)) match {
      case (true, false, false) => Some(-1)
      case (false, true, false) => Some(0)
      case (false, false, true) => Some(1)
      case (false, false, false) => None
      case (_, _, _) => throw new IllegalStateException("")
    }
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.union(that.xs))
  }

  def filter(f: A => Boolean): SetUnionLattice[A] = {
    new SetUnionLattice(xs.filter(f))
  }

  def map[B](f: A => B): SetUnionLattice[B] = {
    new SetUnionLattice(xs.map(f))
  }

  def cross[B](that: SetUnionLattice[B]): SetUnionLattice[(A, B)] = {
    new SetUnionLattice(for (x <- xs; y <- that.xs) yield (x, y))
  }

  def union(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.union(that.xs))
  }

  def intersect(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.intersect(that.xs))
  }

  def diff(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.diff(that.xs))
  }

  def size(): IntMaxLattice = {
    IntMaxLattice(xs.size)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def assign(that: SetUnionLattice[A]) = {
    xs = that.xs
  }

  def addEqual(that: SetUnionLattice[A]) = {
    xs = xs.union(that.xs)
  }

  def subtractequal(that: SetUnionLattice[A]) = {
    xs = xs.diff(that.xs)
  }
}

object SetUnionLattice {
  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr[A] {
    def eval(): SetUnionLattice[A]
    def isMonotonic(): Boolean
    def channels(): Set[fluent.Channel.Existential]

    def filter(f: A => Boolean) = Filter(this, f)
    def map[B](f: A => B) = Map(this, f)
    def cross[B](rhs: Expr[B]) = Cross(this, rhs)
    def union(rhs: Expr[A]) = Union(this, rhs)
    def intersect(rhs: Expr[A]) = Intersect(this, rhs)
    def diff(rhs: Expr[A]) = Diff(this, rhs)
    def size() = IntMaxLattice.Size(this)
  }

  case class Val[A](x: A) extends Expr[A] {
    def eval() = SetUnionLattice(Set(x))
    def isMonotonic() = true
    def channels() = Set()
  }

  case class Channel[A <: AnyRef{val dst: String}](c: fluent.Channel[A]) extends Expr[A] {
    def eval() = SetUnionLattice(c.get)
    def isMonotonic() = true
    def channels() = Set(c)
  }

  case class Const[A](x: SetUnionLattice[A]) extends Expr[A] {
    def eval() = x
    def isMonotonic() = true
    def channels() = Set()
  }

  case class Filter[A](e: Expr[A], f: A => Boolean) extends Expr[A] {
    def eval() = e.eval().filter(f)
    def isMonotonic() = e.isMonotonic()
    def channels() = e.channels()
  }

  case class Map[A, B](e: Expr[A], f: A => B) extends Expr[B] {
    def eval() = e.eval().map(f)
    def isMonotonic() = e.isMonotonic()
    def channels() = e.channels()
  }

  case class Cross[A, B](lhs: Expr[A], rhs: Expr[B]) extends Expr[(A, B)] {
    def eval() = lhs.eval().cross(rhs.eval())
    def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
    def channels() = lhs.channels() ++ rhs.channels()
  }

  case class Union[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().union(rhs.eval())
    def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
    def channels() = lhs.channels() ++ rhs.channels()
  }

  case class Intersect[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().intersect(rhs.eval())
    def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
    def channels() = lhs.channels() ++ rhs.channels()
  }

  case class Diff[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().diff(rhs.eval())
    def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
    def channels() = lhs.channels() ++ rhs.channels()
  }

  implicit def toExpr[A](l: SetUnionLattice[A]): Const[A] = {
    Const(l)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method {
    def isMonotonic(): Boolean
    def isIncreasing(): Boolean
  }

  case object Assign extends Method {
    def isMonotonic() = true
    def isIncreasing() = false
  }

  case object AddEqual extends Method {
    def isMonotonic() = true
    def isIncreasing() = true
  }

  case object SubtractEqual extends Method {
    def isMonotonic() = false
    def isIncreasing() = false
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule[A](l: SetUnionLattice[A], m: Method, e: Expr[A]) {
    def eval() = {
      val v = e.eval()
      m match {
        case Assign => l.assign(v)
        case AddEqual => l.addEqual(v)
        case SubtractEqual => l.subtractequal(v)
      }
    }

    def isMonotonic(): Boolean = m.isMonotonic() && e.isMonotonic()
    def isIncreasing(): Boolean = m.isIncreasing()
    def channels(): Set[fluent.Channel.Existential] = e.channels()
  }

  object Rule {
    implicit def toRule[A](r: Rule[A]): fluent.Rule = SetUnionLatticeRule(r)
  }

  implicit class RuleInfix[A](l: SetUnionLattice[A]) {
    def <--(e: Expr[A]) = Rule(l, Assign, e)
    def +=(e: Expr[A]) = Rule(l, AddEqual, e)
    def -=(e: Expr[A]) = Rule(l, SubtractEqual, e)
  }
}
