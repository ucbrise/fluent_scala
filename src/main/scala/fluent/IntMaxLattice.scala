package fluent

import scala.math
import scala.language.implicitConversions

case class IntMaxLattice(private var x: Int) extends Lattice[IntMaxLattice] {
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: IntMaxLattice): Option[Int] = {
    Some(x.compareTo(that.x))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: IntMaxLattice): IntMaxLattice = {
    new IntMaxLattice(math.max(x, that.x))
  }

  def add(that: IntMaxLattice): IntMaxLattice = {
    IntMaxLattice(x + that.x)
  }

  def subtract(that: IntMaxLattice): IntMaxLattice = {
    IntMaxLattice(x - that.x)
  }

  def greater(x: Int): BoolOrLattice = {
    BoolOrLattice(this.x > x)
  }

  def greaterEqual(x: Int): BoolOrLattice = {
    BoolOrLattice(this.x >= x)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def assign(that: IntMaxLattice) = {
    x = that.x
  }

  def mergeEqual(that: IntMaxLattice) = {
    x = merge(that).x
  }

  def addEqual(that: IntMaxLattice) = {
    x += that.x
  }

  def subtractEqual(that: IntMaxLattice) = {
    x -= that.x
  }
}

object IntMaxLattice {

  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr {
    def eval(): IntMaxLattice
    def isMonotonic(): Boolean

    def +(rhs: Expr) = Add(this, rhs)
    def -(rhs: Expr) = Subtract(this, rhs)
    def >(x: Int) = BoolOrLattice.Greater(this, x)
    def >=(x: Int) = BoolOrLattice.GreaterEqual(this, x)
  }

  case class Const(x: IntMaxLattice) extends Expr {
    override def eval() = x
    override def isMonotonic() = true
  }

  case class Add(lhs: Expr, rhs: Expr) extends Expr {
    override def eval() = lhs.eval() add rhs.eval()
    override def isMonotonic() = lhs.isMonotonic() && rhs.isMonotonic()
  }

  case class Subtract(lhs: Expr, rhs: Expr) extends Expr {
    override def eval() = lhs.eval() subtract rhs.eval()
    override def isMonotonic() = false
  }

  case class Size[A](e: SetUnionLattice.Expr[A]) extends Expr {
    override def eval() = e.eval().size()
    override def isMonotonic() = e.isMonotonic()
  }

  implicit def toConst(l: IntMaxLattice): Const = {
    Const(l)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method {
    def isMonotonic(): Boolean
    def isIncreasing(): Boolean
  }

  case object Assign extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = false
  }

  case object MergeEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = true
  }

  case object AddEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = false
  }

  case object SubtractEqual extends Method {
    override def isMonotonic() = false
    override def isIncreasing() = false
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: IntMaxLattice, m: Method, e: Expr) {
    def eval() = {
      val v = e.eval()
      m match {
        case Assign => l.assign(v)
        case MergeEqual => l.mergeEqual(v)
        case AddEqual => l.addEqual(v)
        case SubtractEqual => l.subtractEqual(v)
      }
    }

    def isMonotonic(): Boolean = m.isMonotonic() && e.isMonotonic()
    def isIncreasing(): Boolean = m.isIncreasing()
    def channels(): Set[Channel.Existential]  = Set()
  }

  object Rule {
    implicit def toRule(r: Rule): fluent.Rule = IntMaxLatticeRule(r)
  }

  implicit class RuleInfix(l: IntMaxLattice) {
    def <--(e: Expr) = Rule(l, Assign, e)
    def <=(e: Expr) = Rule(l, MergeEqual, e)
    def +=(e: Expr) = Rule(l, AddEqual, e)
    def -=(e: Expr) = Rule(l, SubtractEqual, e)
  }
}
