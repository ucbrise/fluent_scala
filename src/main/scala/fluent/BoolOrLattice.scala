package fluent

import scala.language.implicitConversions

case class BoolOrLattice(private var b: Boolean) extends Lattice[BoolOrLattice] {
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: BoolOrLattice): Option[Int] = {
    Some(b.compareTo(that.b))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b || that.b)
  }

  def and(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b && that.b)
  }

  def or(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b || that.b)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def assign(that: BoolOrLattice) = {
    b = that.b
  }

  def andEqual(that: BoolOrLattice) = {
    b &&= that.b
  }

  def orEqual(that: BoolOrLattice) = {
    b ||= that.b
  }
}

object BoolOrLattice {
  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr {
    def eval(): BoolOrLattice
    def isMonotonic(): Boolean

    def &&(rhs: Expr) = And(this, rhs)
    def ||(rhs: Expr) = Or(this, rhs)
  }

  case class Const(x: BoolOrLattice) extends Expr {
    override def eval() = x
    override def isMonotonic() = true
  }

  case class And(lhs: Expr, rhs: Expr) extends Expr {
    override def eval() = lhs.eval() and rhs.eval()
    override def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
  }

  case class Or(lhs: Expr, rhs: Expr) extends Expr {
    override def eval() = lhs.eval() or rhs.eval()
    override def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
  }

  case class Greater(e: IntMaxLattice.Expr, x: Int) extends Expr {
    override def eval() = e.eval() greater x
    override def isMonotonic() = e.isMonotonic()
  }

  case class GreaterEqual(e: IntMaxLattice.Expr, x: Int) extends Expr {
    override def eval() = e.eval() greaterEqual x
    override def isMonotonic() = e.isMonotonic()
  }

  implicit def toConst(l: BoolOrLattice): Const = {
    Const(l)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method {
    def isMonotonic(): Boolean
    def isIncreasing(): Boolean
  }

  case object AssignEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = false
  }

  case object AndEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = false
  }

  case object OrEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = true
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: BoolOrLattice, m: Method, e: Expr) {
    def eval() = {
      val v = e.eval()
      m match {
        case AssignEqual => l.assign(v)
        case AndEqual => l.andEqual(v)
        case OrEqual => l.orEqual(v)
      }
    }

    def isMonotonic(): Boolean = m.isMonotonic() && e.isMonotonic()
    def isIncreasing(): Boolean = m.isMonotonic()
    def channels(): Set[fluent.Channel.Existential] = Set()
  }

  object Rule {
    implicit def toRule(r: Rule): fluent.Rule = BoolOrLatticeRule(r)
  }

  implicit class RuleInfix(l: BoolOrLattice) {
    def <--(e: Expr) = Rule(l, AssignEqual, e)
    def &&=(e: Expr) = Rule(l, AndEqual, e)
    def ||=(e: Expr) = Rule(l, OrEqual, e)
  }
}
