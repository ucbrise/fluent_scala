package fluent

case class BoolOrLattice(private var b: Boolean) extends Lattice[BoolOrLattice] {
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: BoolOrLattice): Option[Int] = {
    Some(b.compareTo(that.b))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b || that.b)
  }

  def &&(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b && that.b)
  }

  def ||(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(b || that.b)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def assign(that: BoolOrLattice) = {
    b = that.b
  }

  def &&=(that: BoolOrLattice) = {
    b &&= that.b
  }

  def ||=(that: BoolOrLattice) = {
    b ||= that.b
  }
}

object BoolOrLattice {
  // Expressions ///////////////////////////////////////////////////////////////
  sealed trait Expr
  case class Const(x: BoolOrLattice) extends Expr
  case class And(lhs: Expr, rhs: Expr) extends Expr
  case class Or(lhs: Expr, rhs: Expr) extends Expr
  case class Greater(e: IntMaxLattice.Expr, x: Int) extends Expr
  case class GreaterEq(e: IntMaxLattice.Expr, x: Int) extends Expr

  object Expr {
    def eval(e: Expr): BoolOrLattice = {
      e match {
        case Const(x) => x
        case And(lhs, rhs) => eval(lhs) && eval(rhs)
        case Or(lhs, rhs) => eval(lhs) || eval(rhs)
        case Greater(e, x) => IntMaxLattice.Expr.eval(e) > x
        case GreaterEq(e, x) => IntMaxLattice.Expr.eval(e) >= x
      }
    }

    def isMonotonic(e: Expr): Boolean = {
      e match {
        case Const(_) => true
        case And(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Or(lhs, rhs) => isMonotonic(lhs) && isMonotonic(rhs)
        case Greater(e, x) => IntMaxLattice.Expr.isMonotonic(e)
        case GreaterEq(e, x) => IntMaxLattice.Expr.isMonotonic(e)
      }
    }
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method
  case object AssignEqual extends Method
  case object AndEqual extends Method
  case object OrEqual extends Method

  object Method {
    def isMonotonic(m: Method): Boolean = {
      m match {
        case AssignEqual => true
        case AndEqual => true
        case OrEqual => true
      }
    }

    def isIncreasing(m: Method): Boolean = {
      m match {
        case AssignEqual => false
        case AndEqual => false
        case OrEqual => true
      }
    }
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: BoolOrLattice, m: Method, e: Expr)
  object Rule {
    def eval(rule: Rule) = {
      val e = Expr.eval(rule.e)
      rule.m match {
        case AssignEqual => rule.l.assign(e)
        case AndEqual => rule.l &&= e
        case OrEqual => rule.l ||= e
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
