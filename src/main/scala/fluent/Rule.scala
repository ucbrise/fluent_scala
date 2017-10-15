package fluent

sealed trait Rule {
  def isMonotonic(): Boolean
  def isIncreasing(): Boolean
  def channels(): Set[Channel.Existential]
}

case class SetUnionLatticeRule[A](r: SetUnionLattice.Rule[A]) extends Rule {
  override def isMonotonic(): Boolean = r.isMonotonic()
  override def isIncreasing(): Boolean = r.isIncreasing()
  override def channels(): Set[Channel.Existential] = r.channels()
}

case class IntMaxLatticeRule(r: IntMaxLattice.Rule) extends Rule {
  override def isMonotonic(): Boolean = r.isMonotonic()
  override def isIncreasing(): Boolean = r.isIncreasing()
  override def channels(): Set[Channel.Existential] = r.channels()
}

case class BoolOrLatticeRule(r: BoolOrLattice.Rule) extends Rule {
  override def isMonotonic(): Boolean = r.isMonotonic()
  override def isIncreasing(): Boolean = r.isIncreasing()
  override def channels(): Set[Channel.Existential] = r.channels()
}

case class ChannelRule[A <: AnyRef{val dst: String}](r: Channel.Rule[A]) extends Rule {
  override def isMonotonic(): Boolean = r.isMonotonic()
  override def isIncreasing(): Boolean = r.isIncreasing()
  override def channels(): Set[Channel.Existential] = r.channels()
}

case class StdOutRule(r: StdOut.Rule) extends Rule {
  override def isMonotonic(): Boolean = r.isMonotonic()
  override def isIncreasing(): Boolean = r.isIncreasing()
  override def channels(): Set[Channel.Existential] = r.channels()
}
