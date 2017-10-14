package fluent

import scala.collection.mutable

case class SetUnionLattice[A](private var xs: mutable.Set[A]) extends Lattice[SetUnionLattice[A]] {
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
  sealed trait Expr[A]
  case class Const[A](x: SetUnionLattice[A]) extends Expr[A]
  case class Filter[A](e: Expr[A], f: A => Boolean) extends Expr[A]
  case class Map[A, B](e: Expr[A], f: A => B) extends Expr[B]
  case class Cross[A, B](lhs: Expr[A], rhs: Expr[B]) extends Expr[(A, B)]
  case class Union[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case class Intersect[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]
  case class Diff[A](lhs: Expr[A], rhs: Expr[A]) extends Expr[A]

  object Expr {
    def eval[A](e: Expr[A]): SetUnionLattice[A] = {
      e match {
        case Const(x) => x
        case Filter(e, f) => eval(e).filter(f)
        case Map(e, f) => eval(e).map(f)
        case Cross(lhs, rhs) => eval(lhs).cross(eval(rhs))
        case Union(lhs, rhs) => eval(lhs).union(eval(rhs))
        case Intersect(lhs, rhs) => eval(lhs).intersect(eval(rhs))
        case Diff(lhs, rhs) => eval(lhs).diff(eval(rhs))
      }
    }

    def isMonotonic[A](e: Expr[A]): Boolean = {
      e match {
        case Const(x) => true
        case Filter(e, _) => isMonotonic(e)
        case Map(e, _) => isMonotonic(e)
        case Cross(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Union(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Intersect(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Diff(_, _) => false
      }
    }
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
      val e = Expr.eval(rule.e)
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
  }
}
