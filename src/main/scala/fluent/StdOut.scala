package fluent

import scala.language.implicitConversions

case class StdOut() {
  def +=(that: SetUnionLattice[String]) = {
    that.xs.foreach(println)
  }
}

object StdOut {
  import SetUnionLattice.Expr

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method
  case object AddEqual extends Method

  object Method {
    def isMonotonic(m: Method): Boolean = {
      m match {
        case AddEqual => true
      }
    }

    def isIncreasing(m: Method): Boolean = {
      m match {
        case AddEqual => true
      }
    }
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: StdOut, m: Method, e: Expr[String])

  object Rule {
    def eval(rule: Rule) = {
      val e = Expr.eval(rule.e)
      rule.m match {
        case AddEqual => rule.l += e
      }
    }

    def isMonotonic(rule: Rule) = {
      Method.isMonotonic(rule.m) && Expr.isMonotonic(rule.e)
    }

    def isIncreasing(rule: Rule) = {
      Method.isIncreasing(rule.m)
    }

    implicit def toRule(r: Rule): fluent.Rule = StdOutRule(r)
  }

  implicit class RuleInfix(l: StdOut) {
    def +=(e: SetUnionLattice.Expr[String]) = Rule(l, AddEqual, e)
  }
}
