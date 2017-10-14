package fluent

import scala.math

case class IntMaxLattice(private var x: Int) extends Lattice[IntMaxLattice] {
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: IntMaxLattice): Option[Int] = {
    Some(x.compareTo(that.x))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: IntMaxLattice): IntMaxLattice = {
    new IntMaxLattice(math.max(x, that.x))
  }

  def +(that: IntMaxLattice): IntMaxLattice = {
    IntMaxLattice(x + that.x)
  }

  def -(that: IntMaxLattice): IntMaxLattice = {
    IntMaxLattice(x - that.x)
  }

  def >(x: Int): BoolOrLattice = {
    BoolOrLattice(this.x > x)
  }

  def >=(x: Int): BoolOrLattice = {
    BoolOrLattice(this.x >= x)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def assign(that: IntMaxLattice) = {
    x = that.x
  }

  def mergeEqual(that: IntMaxLattice) = {
    x = merge(that).x
  }

  def +=(that: IntMaxLattice) = {
    x += that.x
  }

  def -=(that: IntMaxLattice) = {
    x -= that.x
  }
}

object IntMaxLattice {
  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr
  case class Const(x: IntMaxLattice) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Subtract(lhs: Expr, rhs: Expr) extends Expr
  case class Size[A](e: SetUnionLattice.Expr[A]) extends Expr

  object Expr {
    def eval(e: Expr): IntMaxLattice = {
      e match {
        case Const(x) => x
        case Add(lhs, rhs) => eval(lhs) + eval(rhs)
        case Subtract(lhs, rhs) => eval(lhs) - eval(rhs)
        case Size(x) => SetUnionLattice.Expr.eval(x).size()
      }
    }

    def isMonotonic(e: Expr): Boolean = {
      e match {
        case Const(_) => true
        case Add(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Subtract(_, _) => false
        case Size(e) => SetUnionLattice.Expr.isMonotonic(e)
      }
    }
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method
  case object AssignEqual extends Method
  case object MergeEqual extends Method
  case object AddEqual extends Method
  case object SubtractEqual extends Method

  object Method {
    def isMonotonic(m: Method): Boolean = {
      m match {
        case AssignEqual => true
        case MergeEqual => true
        case AddEqual => true
        case SubtractEqual => false
      }
    }

    def isIncreasing(m: Method): Boolean = {
      m match {
        case AssignEqual => false
        case MergeEqual => true
        case AddEqual => false
        case SubtractEqual => false
      }
    }
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: IntMaxLattice, m: Method, e: Expr)
  object Rule {
    def eval(rule: Rule) = {
      val e = Expr.eval(rule.e)
      rule.m match {
        case AssignEqual => rule.l.assign(e)
        case MergeEqual => rule.l.mergeEqual(e)
        case AddEqual => rule.l += e
        case SubtractEqual => rule.l -= e
      }
    }

    def isMonotonic(rule: Rule) = {
      Method.isMonotonic(rule.m) && Expr.isMonotonic(rule.e)
    }

    def isIncreasing(rule: Rule) = {
      Method.isIncreasing(rule.m)
    }
  }
}
