package fluent

import scala.language.implicitConversions

case class StdOut() {
  def addEqual(that: SetUnionLattice[String]) = {
    that.xs.foreach(println)
  }
}

object StdOut {
  import SetUnionLattice.Expr

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method {
    def isMonotonic(): Boolean
    def isIncreasing(): Boolean
  }

  case object AddEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = true
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule(l: StdOut, m: Method, e: Expr[String]) {
    def eval() = {
      val v = e.eval()
      m match {
        case AddEqual => l.addEqual(v)
      }
    }

    def isMonotonic(): Boolean = m.isMonotonic() && e.isMonotonic()
    def isIncreasing(): Boolean = m.isIncreasing()
    def channels(): Set[Channel.Existential] = e.channels()
  }

  object Rule {
    implicit def toRule(r: Rule): fluent.Rule = StdOutRule(r)
  }

  implicit class RuleInfix(l: StdOut) {
    def +=(e: SetUnionLattice.Expr[String]) = Rule(l, AddEqual, e)
  }
}
