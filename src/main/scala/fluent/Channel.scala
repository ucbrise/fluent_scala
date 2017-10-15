package fluent

import scala.language.implicitConversions

case class Channel[A <: AnyRef{val dst: String}](val name: String) {
  private var xs: Set[A] = Set()

  def get() = xs
  def clear() = {xs = Set()}

  def assign(that: SetUnionLattice[A]) = {xs = that.xs}
  def addEqual(xs: Set[A]) = {this.xs = this.xs.union(xs)}
  def subtractEqual(that: SetUnionLattice[A]) = {xs = xs.diff(that.xs)}
}

object Channel {
  import SetUnionLattice.Expr

  type Existential = Channel[T] forSome {type T <: AnyRef{val dst: String}};

  // Expressions ///////////////////////////////////////////////////////////////
  implicit def toExpr[A <: AnyRef{val dst: String}](c: Channel[A]): SetUnionLattice.Channel[A] = {
    SetUnionLattice.Channel(c)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  sealed trait Method {
    def isMonotonic(): Boolean
    def isIncreasing(): Boolean
  }

  case object AddEqual extends Method {
    override def isMonotonic() = true
    override def isIncreasing() = true
  }

  case object SubtractEqual extends Method {
    override def isMonotonic() = false
    override def isIncreasing() = false
  }

  // Rules /////////////////////////////////////////////////////////////////////
  case class Rule[A <: AnyRef{val dst: String}](l: Channel[A], m: Method, e: Expr[A]) {
    def isMonotonic() = m.isMonotonic() && e.isMonotonic()
    def isIncreasing() = m.isIncreasing()
    def channels(): Set[fluent.Channel.Existential] = e.channels()
  }

  object Rule {
    implicit def toRule[A <: AnyRef{val dst: String}](r: Rule[A]): fluent.Rule = {
      ChannelRule(r)
    }
  }

  implicit class RuleInfix[A <: AnyRef{val dst: String}](l: Channel[A]) {
    def +=(e: SetUnionLattice.Expr[A]) = Rule(l, AddEqual, e)
    def -=(e: SetUnionLattice.Expr[A]) = Rule(l, SubtractEqual, e)
  }
}
