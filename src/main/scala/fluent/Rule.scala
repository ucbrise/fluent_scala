package fluent

sealed trait Rule
case class SetUnionLatticeRule[A](r: SetUnionLattice.Rule[A]) extends Rule
case class IntMaxLatticeRule(r: IntMaxLattice.Rule) extends Rule
case class BoolOrLatticeRule(r: BoolOrLattice.Rule) extends Rule
case class ChannelRule[A <: AnyRef{val dst: String}](r: Channel.Rule[A]) extends Rule
case class StdOutRule(r: StdOut.Rule) extends Rule

object Rule {
  def isMonotonic(rule: Rule): Boolean = {
    rule match {
      case SetUnionLatticeRule(r) => SetUnionLattice.Rule.isMonotonic(r)
      case IntMaxLatticeRule(r) => IntMaxLattice.Rule.isMonotonic(r)
      case BoolOrLatticeRule(r) => BoolOrLattice.Rule.isMonotonic(r)
      case ChannelRule(r) => Channel.Rule.isMonotonic(r)
      case StdOutRule(r) => StdOut.Rule.isMonotonic(r)
    }
  }

  def isIncreasing(rule: Rule): Boolean = {
    rule match {
      case SetUnionLatticeRule(r) => SetUnionLattice.Rule.isIncreasing(r)
      case IntMaxLatticeRule(r) => IntMaxLattice.Rule.isIncreasing(r)
      case BoolOrLatticeRule(r) => BoolOrLattice.Rule.isIncreasing(r)
      case ChannelRule(r) => Channel.Rule.isIncreasing(r)
      case StdOutRule(r) => StdOut.Rule.isIncreasing(r)
    }
  }

  def channels(rule: Rule): Set[Channel.Existential] = {
    rule match {
      case SetUnionLatticeRule(r) => SetUnionLattice.Rule.channels(r)
      case IntMaxLatticeRule(r) => IntMaxLattice.Rule.channels(r)
      case BoolOrLatticeRule(r) => BoolOrLattice.Rule.channels(r)
      case ChannelRule(r) => Channel.Rule.channels(r)
      case StdOutRule(r) => StdOut.Rule.channels(r)
    }
  }
}
