package fluent

import scala.language.implicitConversions

case class Channel[A <: AnyRef{val dst: String}](val name: String) {
  private var xs: Set[A] = Set()
  def assign(that: SetUnionLattice[A]) = {xs = that.xs}
  def +=(xs: Set[A]) = {this.xs = this.xs.union(xs)}
  def -=(that: SetUnionLattice[A]) = {xs = xs.diff(that.xs)}
  def clear() = {xs = Set()}
}

object Channel {
  import SetUnionLattice.Expr

  // Expressions ///////////////////////////////////////////////////////////////
  implicit def toExpr[A <: AnyRef{val dst: String}](c: Channel[A]): SetUnionLattice.Const[A] = {
    SetUnionLattice.Const(SetUnionLattice(c.xs))
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method
  case object AddEqual extends Method
  case object SubtractEqual extends Method

  object Method {
    def isMonotonic(m: Method): Boolean = {
      m match {
        case AddEqual => true
        case SubtractEqual => false
      }
    }

    def isIncreasing(m: Method): Boolean = {
      m match {
        case AddEqual => true
        case SubtractEqual => false
      }
    }
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule[A <: AnyRef{val dst: String}](
    l: Channel[A],
    m: Method,
    e: Expr[A])

  object Rule {
    def isMonotonic[A <: AnyRef{val dst: String}](rule: Rule[A]) = {
      Method.isMonotonic(rule.m) && Expr.isMonotonic(rule.e)
    }

    def isIncreasing[A <: AnyRef{val dst: String}](rule: Rule[A]) = {
      Method.isIncreasing(rule.m)
    }

    implicit def toRule[A <: AnyRef{val dst: String}](r: Rule[A]): fluent.Rule = {
      ChannelRule(r)
    }
  }

  implicit class RuleInfix[A <: AnyRef{val dst: String}](l: Channel[A]) {
    def +=(e: SetUnionLattice.Expr[A]) = Rule(l, AddEqual, e)
    def -=(e: SetUnionLattice.Expr[A]) = Rule(l, SubtractEqual, e)
  }
}
