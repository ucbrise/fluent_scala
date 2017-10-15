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

  def +=(that: SetUnionLattice[A]) = {
    xs = xs.union(that.xs)
  }

  def -=(that: SetUnionLattice[A]) = {
    xs = xs.diff(that.xs)
  }
}

object SetUnionLattice {
  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr[A] {
    def eval(): SetUnionLattice[A]
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
    def channels() = Set()
  }
  case class Channel[A <: AnyRef{val dst: String}](c: fluent.Channel[A]) extends Expr[A] {
    def eval() = SetUnionLattice(c.get)
    def channels() = Set(c)
  }
  case class Const[A](x: SetUnionLattice[A]) extends Expr[A] {
    def eval() = x
    def channels() = Set()
  }
  case class Filter[A](e: Expr[A], f: A => Boolean) extends Expr[A] {
    def eval() = e.eval().filter(f)
    def channels() = e.channels()
  }
  case class Map[A, B](e: Expr[A], f: A => B) extends Expr[B] {
    def eval() = e.eval().map(f)
    def channels() = e.channels()
  }
  case class Cross[A, B](lhs: Expr[A], rhs: Expr[B]) extends Expr[(A, B)] {
    def eval() = lhs.eval().cross(rhs.eval())
    def channels() = lhs.channels() ++ rhs.channels()
  }
  case class Union[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().union(rhs.eval())
    def channels() = lhs.channels() ++ rhs.channels()
  }
  case class Intersect[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().intersect(rhs.eval())
    def channels() = lhs.channels() ++ rhs.channels()
  }
  case class Diff[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A] {
    def eval() = lhs.eval().diff(rhs.eval())
    def channels() = lhs.channels() ++ rhs.channels()
  }

  object Expr {
    def eval[A](e: Expr[A]): SetUnionLattice[A] = {
      e.eval()
    }

    def isMonotonic[A](e: Expr[A]): Boolean = {
      e match {
        case Val(_) => true
        case Channel(_) => true
        case Const(_) => true
        case Filter(e, _) => isMonotonic(e)
        case Map(e, _) => isMonotonic(e)
        case Cross(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Union(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Intersect(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Diff(_, _) => false
      }
    }
  }

  implicit def toExpr[A](l: SetUnionLattice[A]): Const[A] = {
    Const(l)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method
  case object AssignEqual extends Method
  case object AddEqual extends Method
  case object SubtractEqual extends Method

  object Method {
    def isMonotonic(m: Method): Boolean = {
      m match {
        case AssignEqual => true
        case AddEqual => true
        case SubtractEqual => false
      }
    }

    def isIncreasing(m: Method): Boolean = {
      m match {
        case AssignEqual => false
        case AddEqual => true
        case SubtractEqual => false
      }
    }
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule[A](l: SetUnionLattice[A], m: Method, e: Expr[A])

  object Rule {
    def eval[A](rule: Rule[A]) = {
      val e = rule.e.eval()
      rule.m match {
        case AssignEqual => rule.l.assign(e)
        case AddEqual => rule.l += e
        case SubtractEqual => rule.l -= e
      }
    }

    def isMonotonic[A](rule: Rule[A]) = {
      Method.isMonotonic(rule.m) && Expr.isMonotonic(rule.e)
    }

    def isIncreasing[A](rule: Rule[A]) = {
      Method.isIncreasing(rule.m)
    }

    def channels[A](rule: Rule[A]): Set[fluent.Channel.Existential] = {
      rule.e.channels()
    }

    implicit def toRule[A](r: Rule[A]): fluent.Rule = SetUnionLatticeRule(r)
  }

  implicit class RuleInfix[A](l: SetUnionLattice[A]) {
    def <--(e: Expr[A]) = Rule(l, AssignEqual, e)
    def +=(e: Expr[A]) = Rule(l, AddEqual, e)
    def -=(e: Expr[A]) = Rule(l, SubtractEqual, e)
  }
}
